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
-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified inline policy that is embedded in the specified IAM group.
--
--
-- A group can also have managed policies attached to it. To detach a managed policy from a group, use 'DetachGroupPolicy' . For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
module Network.AWS.IAM.DeleteGroupPolicy
    (
    -- * Creating a Request
      deleteGroupPolicy
    , DeleteGroupPolicy
    -- * Request Lenses
    , dGroupName
    , dPolicyName

    -- * Destructuring the Response
    , deleteGroupPolicyResponse
    , DeleteGroupPolicyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGroupPolicy' smart constructor.
data DeleteGroupPolicy = DeleteGroupPolicy'
  { _dGroupName  :: !Text
  , _dPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGroupName' - The name (friendly name, not ARN) identifying the group that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dPolicyName' - The name identifying the policy document to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteGroupPolicy
    :: Text -- ^ 'dGroupName'
    -> Text -- ^ 'dPolicyName'
    -> DeleteGroupPolicy
deleteGroupPolicy pGroupName_ pPolicyName_ =
  DeleteGroupPolicy' {_dGroupName = pGroupName_, _dPolicyName = pPolicyName_}


-- | The name (friendly name, not ARN) identifying the group that the policy is embedded in. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dGroupName :: Lens' DeleteGroupPolicy Text
dGroupName = lens _dGroupName (\ s a -> s{_dGroupName = a})

-- | The name identifying the policy document to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dPolicyName :: Lens' DeleteGroupPolicy Text
dPolicyName = lens _dPolicyName (\ s a -> s{_dPolicyName = a})

instance AWSRequest DeleteGroupPolicy where
        type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
        request = postQuery iam
        response = receiveNull DeleteGroupPolicyResponse'

instance Hashable DeleteGroupPolicy where

instance NFData DeleteGroupPolicy where

instance ToHeaders DeleteGroupPolicy where
        toHeaders = const mempty

instance ToPath DeleteGroupPolicy where
        toPath = const "/"

instance ToQuery DeleteGroupPolicy where
        toQuery DeleteGroupPolicy'{..}
          = mconcat
              ["Action" =: ("DeleteGroupPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _dGroupName,
               "PolicyName" =: _dPolicyName]

-- | /See:/ 'deleteGroupPolicyResponse' smart constructor.
data DeleteGroupPolicyResponse =
  DeleteGroupPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGroupPolicyResponse' with the minimum fields required to make a request.
--
deleteGroupPolicyResponse
    :: DeleteGroupPolicyResponse
deleteGroupPolicyResponse = DeleteGroupPolicyResponse'


instance NFData DeleteGroupPolicyResponse where
