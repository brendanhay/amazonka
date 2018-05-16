{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes AWS OpsWorks Stacks service errors.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
-- This call accepts only one resource-identifying parameter.
--
module Network.AWS.OpsWorks.DescribeServiceErrors
    (
    -- * Creating a Request
      describeServiceErrors
    , DescribeServiceErrors
    -- * Request Lenses
    , dseInstanceId
    , dseStackId
    , dseServiceErrorIds

    -- * Destructuring the Response
    , describeServiceErrorsResponse
    , DescribeServiceErrorsResponse
    -- * Response Lenses
    , dsersServiceErrors
    , dsersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServiceErrors' smart constructor.
data DescribeServiceErrors = DescribeServiceErrors'
  { _dseInstanceId      :: !(Maybe Text)
  , _dseStackId         :: !(Maybe Text)
  , _dseServiceErrorIds :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServiceErrors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseInstanceId' - The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
--
-- * 'dseStackId' - The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
--
-- * 'dseServiceErrorIds' - An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
describeServiceErrors
    :: DescribeServiceErrors
describeServiceErrors =
  DescribeServiceErrors'
    { _dseInstanceId = Nothing
    , _dseStackId = Nothing
    , _dseServiceErrorIds = Nothing
    }


-- | The instance ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified instance.
dseInstanceId :: Lens' DescribeServiceErrors (Maybe Text)
dseInstanceId = lens _dseInstanceId (\ s a -> s{_dseInstanceId = a})

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the errors associated with the specified stack.
dseStackId :: Lens' DescribeServiceErrors (Maybe Text)
dseStackId = lens _dseStackId (\ s a -> s{_dseStackId = a})

-- | An array of service error IDs. If you use this parameter, @DescribeServiceErrors@ returns descriptions of the specified errors. Otherwise, it returns a description of every error.
dseServiceErrorIds :: Lens' DescribeServiceErrors [Text]
dseServiceErrorIds = lens _dseServiceErrorIds (\ s a -> s{_dseServiceErrorIds = a}) . _Default . _Coerce

instance AWSRequest DescribeServiceErrors where
        type Rs DescribeServiceErrors =
             DescribeServiceErrorsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServiceErrorsResponse' <$>
                   (x .?> "ServiceErrors" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeServiceErrors where

instance NFData DescribeServiceErrors where

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
              (catMaybes
                 [("InstanceId" .=) <$> _dseInstanceId,
                  ("StackId" .=) <$> _dseStackId,
                  ("ServiceErrorIds" .=) <$> _dseServiceErrorIds])

instance ToPath DescribeServiceErrors where
        toPath = const "/"

instance ToQuery DescribeServiceErrors where
        toQuery = const mempty

-- | Contains the response to a @DescribeServiceErrors@ request.
--
--
--
-- /See:/ 'describeServiceErrorsResponse' smart constructor.
data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'
  { _dsersServiceErrors  :: !(Maybe [ServiceError'])
  , _dsersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServiceErrorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsersServiceErrors' - An array of @ServiceError@ objects that describe the specified service errors.
--
-- * 'dsersResponseStatus' - -- | The response status code.
describeServiceErrorsResponse
    :: Int -- ^ 'dsersResponseStatus'
    -> DescribeServiceErrorsResponse
describeServiceErrorsResponse pResponseStatus_ =
  DescribeServiceErrorsResponse'
    {_dsersServiceErrors = Nothing, _dsersResponseStatus = pResponseStatus_}


-- | An array of @ServiceError@ objects that describe the specified service errors.
dsersServiceErrors :: Lens' DescribeServiceErrorsResponse [ServiceError']
dsersServiceErrors = lens _dsersServiceErrors (\ s a -> s{_dsersServiceErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
dsersResponseStatus :: Lens' DescribeServiceErrorsResponse Int
dsersResponseStatus = lens _dsersResponseStatus (\ s a -> s{_dsersResponseStatus = a})

instance NFData DescribeServiceErrorsResponse where
