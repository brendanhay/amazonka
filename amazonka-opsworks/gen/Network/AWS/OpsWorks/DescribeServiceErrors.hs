{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes AWS OpsWorks service errors.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeServiceErrors.html>
module Network.AWS.OpsWorks.DescribeServiceErrors
    (
    -- * Request
      DescribeServiceErrors
    -- ** Request constructor
    , describeServiceErrors
    -- ** Request lenses
    , dseInstanceId
    , dseServiceErrorIds
    , dseStackId

    -- * Response
    , DescribeServiceErrorsResponse
    -- ** Response constructor
    , describeServiceErrorsResponse
    -- ** Response lenses
    , dserServiceErrors
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServiceErrors' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dseInstanceId'
--
-- * 'dseServiceErrorIds'
--
-- * 'dseStackId'
data DescribeServiceErrors = DescribeServiceErrors'{_dseInstanceId :: Maybe Text, _dseServiceErrorIds :: Maybe [Text], _dseStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeServiceErrors' smart constructor.
describeServiceErrors :: DescribeServiceErrors
describeServiceErrors = DescribeServiceErrors'{_dseInstanceId = Nothing, _dseServiceErrorIds = Nothing, _dseStackId = Nothing};

-- | The instance ID. If you use this parameter, @DescribeServiceErrors@
-- returns descriptions of the errors associated with the specified
-- instance.
dseInstanceId :: Lens' DescribeServiceErrors (Maybe Text)
dseInstanceId = lens _dseInstanceId (\ s a -> s{_dseInstanceId = a});

-- | An array of service error IDs. If you use this parameter,
-- @DescribeServiceErrors@ returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
dseServiceErrorIds :: Lens' DescribeServiceErrors [Text]
dseServiceErrorIds = lens _dseServiceErrorIds (\ s a -> s{_dseServiceErrorIds = a}) . _Default;

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
-- descriptions of the errors associated with the specified stack.
dseStackId :: Lens' DescribeServiceErrors (Maybe Text)
dseStackId = lens _dseStackId (\ s a -> s{_dseStackId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeServiceErrors where
        type Sv DescribeServiceErrors = OpsWorks
        type Rs DescribeServiceErrors =
             DescribeServiceErrorsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServiceErrorsResponse' <$>
                   (x .?> "ServiceErrors" .!@ mempty))

instance ToHeaders DescribeServiceErrors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeServiceErrors" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServiceErrors where
        toJSON DescribeServiceErrors'{..}
          = object
              ["InstanceId" .= _dseInstanceId,
               "ServiceErrorIds" .= _dseServiceErrorIds,
               "StackId" .= _dseStackId]

instance ToPath DescribeServiceErrors where
        toPath = const "/"

instance ToQuery DescribeServiceErrors where
        toQuery = const mempty

-- | /See:/ 'describeServiceErrorsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dserServiceErrors'
newtype DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'{_dserServiceErrors :: Maybe [ServiceError']} deriving (Eq, Read, Show)

-- | 'DescribeServiceErrorsResponse' smart constructor.
describeServiceErrorsResponse :: DescribeServiceErrorsResponse
describeServiceErrorsResponse = DescribeServiceErrorsResponse'{_dserServiceErrors = Nothing};

-- | An array of @ServiceError@ objects that describe the specified service
-- errors.
dserServiceErrors :: Lens' DescribeServiceErrorsResponse [ServiceError']
dserServiceErrors = lens _dserServiceErrors (\ s a -> s{_dserServiceErrors = a}) . _Default;
