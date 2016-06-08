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
-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the policy that grants an entity permission to assume a role. For more information about roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities>.
module Network.AWS.IAM.UpdateAssumeRolePolicy
    (
    -- * Creating a Request
      updateAssumeRolePolicy
    , UpdateAssumeRolePolicy
    -- * Request Lenses
    , uarpRoleName
    , uarpPolicyDocument

    -- * Destructuring the Response
    , updateAssumeRolePolicyResponse
    , UpdateAssumeRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAssumeRolePolicy' smart constructor.
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy'
    { _uarpRoleName       :: !Text
    , _uarpPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAssumeRolePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarpRoleName'
--
-- * 'uarpPolicyDocument'
updateAssumeRolePolicy
    :: Text -- ^ 'uarpRoleName'
    -> Text -- ^ 'uarpPolicyDocument'
    -> UpdateAssumeRolePolicy
updateAssumeRolePolicy pRoleName_ pPolicyDocument_ =
    UpdateAssumeRolePolicy'
    { _uarpRoleName = pRoleName_
    , _uarpPolicyDocument = pPolicyDocument_
    }

-- | The name of the role to update.
uarpRoleName :: Lens' UpdateAssumeRolePolicy Text
uarpRoleName = lens _uarpRoleName (\ s a -> s{_uarpRoleName = a});

-- | The policy that grants an entity permission to assume the role.
uarpPolicyDocument :: Lens' UpdateAssumeRolePolicy Text
uarpPolicyDocument = lens _uarpPolicyDocument (\ s a -> s{_uarpPolicyDocument = a});

instance AWSRequest UpdateAssumeRolePolicy where
        type Rs UpdateAssumeRolePolicy =
             UpdateAssumeRolePolicyResponse
        request = postQuery iam
        response
          = receiveNull UpdateAssumeRolePolicyResponse'

instance Hashable UpdateAssumeRolePolicy

instance NFData UpdateAssumeRolePolicy

instance ToHeaders UpdateAssumeRolePolicy where
        toHeaders = const mempty

instance ToPath UpdateAssumeRolePolicy where
        toPath = const "/"

instance ToQuery UpdateAssumeRolePolicy where
        toQuery UpdateAssumeRolePolicy'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAssumeRolePolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _uarpRoleName,
               "PolicyDocument" =: _uarpPolicyDocument]

-- | /See:/ 'updateAssumeRolePolicyResponse' smart constructor.
data UpdateAssumeRolePolicyResponse =
    UpdateAssumeRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAssumeRolePolicyResponse' with the minimum fields required to make a request.
--
updateAssumeRolePolicyResponse
    :: UpdateAssumeRolePolicyResponse
updateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'

instance NFData UpdateAssumeRolePolicyResponse
