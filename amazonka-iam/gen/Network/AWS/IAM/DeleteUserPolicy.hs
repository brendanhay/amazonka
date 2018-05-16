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
-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM user.
--
--
-- A user can also have managed policies attached to it. To detach a managed policy from a user, use 'DetachUserPolicy' . For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DeleteUserPolicy
    (
    -- * Creating a Request
      deleteUserPolicy
    , DeleteUserPolicy
    -- * Request Lenses
    , dupUserName
    , dupPolicyName

    -- * Destructuring the Response
    , deleteUserPolicyResponse
    , DeleteUserPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserPolicy' smart constructor.
data DeleteUserPolicy = DeleteUserPolicy'
  { _dupUserName   :: !Text
  , _dupPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupUserName' - The name (friendly name, not ARN) identifying the user that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dupPolicyName' - The name identifying the policy document to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteUserPolicy
    :: Text -- ^ 'dupUserName'
    -> Text -- ^ 'dupPolicyName'
    -> DeleteUserPolicy
deleteUserPolicy pUserName_ pPolicyName_ =
  DeleteUserPolicy' {_dupUserName = pUserName_, _dupPolicyName = pPolicyName_}


-- | The name (friendly name, not ARN) identifying the user that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dupUserName :: Lens' DeleteUserPolicy Text
dupUserName = lens _dupUserName (\ s a -> s{_dupUserName = a})

-- | The name identifying the policy document to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dupPolicyName :: Lens' DeleteUserPolicy Text
dupPolicyName = lens _dupPolicyName (\ s a -> s{_dupPolicyName = a})

instance AWSRequest DeleteUserPolicy where
        type Rs DeleteUserPolicy = DeleteUserPolicyResponse
        request = postQuery iam
        response = receiveNull DeleteUserPolicyResponse'

instance Hashable DeleteUserPolicy where

instance NFData DeleteUserPolicy where

instance ToHeaders DeleteUserPolicy where
        toHeaders = const mempty

instance ToPath DeleteUserPolicy where
        toPath = const "/"

instance ToQuery DeleteUserPolicy where
        toQuery DeleteUserPolicy'{..}
          = mconcat
              ["Action" =: ("DeleteUserPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dupUserName,
               "PolicyName" =: _dupPolicyName]

-- | /See:/ 'deleteUserPolicyResponse' smart constructor.
data DeleteUserPolicyResponse =
  DeleteUserPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPolicyResponse' with the minimum fields required to make a request.
--
deleteUserPolicyResponse
    :: DeleteUserPolicyResponse
deleteUserPolicyResponse = DeleteUserPolicyResponse'


instance NFData DeleteUserPolicyResponse where
