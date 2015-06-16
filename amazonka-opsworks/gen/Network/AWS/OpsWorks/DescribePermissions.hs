{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.DescribePermissions
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

-- | Describes the permissions for a specified stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribePermissions.html>
module Network.AWS.OpsWorks.DescribePermissions
    (
    -- * Request
      DescribePermissions
    -- ** Request constructor
    , describePermissions
    -- ** Request lenses
    , dpIAMUserARN
    , dpStackId

    -- * Response
    , DescribePermissionsResponse
    -- ** Response constructor
    , describePermissionsResponse
    -- ** Response lenses
    , dprPermissions
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'describePermissions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpIAMUserARN'
--
-- * 'dpStackId'
data DescribePermissions = DescribePermissions'{_dpIAMUserARN :: Maybe Text, _dpStackId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribePermissions' smart constructor.
describePermissions :: DescribePermissions
describePermissions = DescribePermissions'{_dpIAMUserARN = Nothing, _dpStackId = Nothing};

-- | The user\'s IAM ARN. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
dpIAMUserARN :: Lens' DescribePermissions (Maybe Text)
dpIAMUserARN = lens _dpIAMUserARN (\ s a -> s{_dpIAMUserARN = a});

-- | The stack ID.
dpStackId :: Lens' DescribePermissions (Maybe Text)
dpStackId = lens _dpStackId (\ s a -> s{_dpStackId = a});

instance AWSRequest DescribePermissions where
        type Sv DescribePermissions = OpsWorks
        type Rs DescribePermissions =
             DescribePermissionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribePermissionsResponse' <$>
                   (x .?> "Permissions" .!@ mempty))

instance ToHeaders DescribePermissions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribePermissions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePermissions where
        toJSON DescribePermissions'{..}
          = object
              ["IamUserArn" .= _dpIAMUserARN,
               "StackId" .= _dpStackId]

instance ToPath DescribePermissions where
        toPath = const "/"

instance ToQuery DescribePermissions where
        toQuery = const mempty

-- | /See:/ 'describePermissionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprPermissions'
newtype DescribePermissionsResponse = DescribePermissionsResponse'{_dprPermissions :: Maybe [Permission]} deriving (Eq, Read, Show)

-- | 'DescribePermissionsResponse' smart constructor.
describePermissionsResponse :: DescribePermissionsResponse
describePermissionsResponse = DescribePermissionsResponse'{_dprPermissions = Nothing};

-- | An array of @Permission@ objects that describe the stack permissions.
--
-- -   If the request object contains only a stack ID, the array contains a
--     @Permission@ object with permissions for each of the stack IAM ARNs.
-- -   If the request object contains only an IAM ARN, the array contains a
--     @Permission@ object with permissions for each of the user\'s stack
--     IDs.
-- -   If the request contains a stack ID and an IAM ARN, the array
--     contains a single @Permission@ object with permissions for the
--     specified stack and IAM ARN.
dprPermissions :: Lens' DescribePermissionsResponse [Permission]
dprPermissions = lens _dprPermissions (\ s a -> s{_dprPermissions = a}) . _Default;
