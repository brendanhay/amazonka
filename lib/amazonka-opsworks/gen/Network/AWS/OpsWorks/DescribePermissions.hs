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
-- Module      : Network.AWS.OpsWorks.DescribePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a specified stack.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribePermissions
    (
    -- * Creating a Request
      describePermissions
    , DescribePermissions
    -- * Request Lenses
    , dpIAMUserARN
    , dpStackId

    -- * Destructuring the Response
    , describePermissionsResponse
    , DescribePermissionsResponse
    -- * Response Lenses
    , dprsPermissions
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePermissions' smart constructor.
data DescribePermissions = DescribePermissions'
  { _dpIAMUserARN :: !(Maybe Text)
  , _dpStackId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpIAMUserARN' - The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'dpStackId' - The stack ID.
describePermissions
    :: DescribePermissions
describePermissions =
  DescribePermissions' {_dpIAMUserARN = Nothing, _dpStackId = Nothing}


-- | The user's IAM ARN. This can also be a federated user's ARN. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
dpIAMUserARN :: Lens' DescribePermissions (Maybe Text)
dpIAMUserARN = lens _dpIAMUserARN (\ s a -> s{_dpIAMUserARN = a})

-- | The stack ID.
dpStackId :: Lens' DescribePermissions (Maybe Text)
dpStackId = lens _dpStackId (\ s a -> s{_dpStackId = a})

instance AWSRequest DescribePermissions where
        type Rs DescribePermissions =
             DescribePermissionsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribePermissionsResponse' <$>
                   (x .?> "Permissions" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribePermissions where

instance NFData DescribePermissions where

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
              (catMaybes
                 [("IamUserArn" .=) <$> _dpIAMUserARN,
                  ("StackId" .=) <$> _dpStackId])

instance ToPath DescribePermissions where
        toPath = const "/"

instance ToQuery DescribePermissions where
        toQuery = const mempty

-- | Contains the response to a @DescribePermissions@ request.
--
--
--
-- /See:/ 'describePermissionsResponse' smart constructor.
data DescribePermissionsResponse = DescribePermissionsResponse'
  { _dprsPermissions    :: !(Maybe [Permission])
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsPermissions' - An array of @Permission@ objects that describe the stack permissions.     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePermissionsResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribePermissionsResponse
describePermissionsResponse pResponseStatus_ =
  DescribePermissionsResponse'
    {_dprsPermissions = Nothing, _dprsResponseStatus = pResponseStatus_}


-- | An array of @Permission@ objects that describe the stack permissions.     * If the request object contains only a stack ID, the array contains a @Permission@ object with permissions for each of the stack IAM ARNs.     * If the request object contains only an IAM ARN, the array contains a @Permission@ object with permissions for each of the user's stack IDs.     * If the request contains a stack ID and an IAM ARN, the array contains a single @Permission@ object with permissions for the specified stack and IAM ARN.
dprsPermissions :: Lens' DescribePermissionsResponse [Permission]
dprsPermissions = lens _dprsPermissions (\ s a -> s{_dprsPermissions = a}) . _Default . _Coerce

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePermissionsResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribePermissionsResponse where
