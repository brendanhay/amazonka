{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified
-- user.
--
-- A user can also have managed policies attached to it. To detach a
-- managed policy from a user, use DetachUserPolicy. For more information
-- about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUserPolicy.html>
module Network.AWS.IAM.DeleteUserPolicy
    (
    -- * Request
      DeleteUserPolicy
    -- ** Request constructor
    , deleteUserPolicy
    -- ** Request lenses
    , dupUserName
    , dupPolicyName

    -- * Response
    , DeleteUserPolicyResponse
    -- ** Response constructor
    , deleteUserPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteUserPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupUserName'
--
-- * 'dupPolicyName'
data DeleteUserPolicy = DeleteUserPolicy'
    { _dupUserName   :: !Text
    , _dupPolicyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUserPolicy' smart constructor.
deleteUserPolicy :: Text -> Text -> DeleteUserPolicy
deleteUserPolicy pUserName_ pPolicyName_ =
    DeleteUserPolicy'
    { _dupUserName = pUserName_
    , _dupPolicyName = pPolicyName_
    }

-- | The name (friendly name, not ARN) identifying the user that the policy
-- is embedded in.
dupUserName :: Lens' DeleteUserPolicy Text
dupUserName = lens _dupUserName (\ s a -> s{_dupUserName = a});

-- | The name identifying the policy document to delete.
dupPolicyName :: Lens' DeleteUserPolicy Text
dupPolicyName = lens _dupPolicyName (\ s a -> s{_dupPolicyName = a});

instance AWSRequest DeleteUserPolicy where
        type Sv DeleteUserPolicy = IAM
        type Rs DeleteUserPolicy = DeleteUserPolicyResponse
        request = post "DeleteUserPolicy"
        response = receiveNull DeleteUserPolicyResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteUserPolicyResponse' smart constructor.
deleteUserPolicyResponse :: DeleteUserPolicyResponse
deleteUserPolicyResponse = DeleteUserPolicyResponse'
