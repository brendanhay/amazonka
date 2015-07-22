{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the policy that grants an entity permission to assume a role.
-- For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html Using Roles to Delegate Permissions and Federate Identities>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAssumeRolePolicy.html>
module Network.AWS.IAM.UpdateAssumeRolePolicy
    (
    -- * Request
      UpdateAssumeRolePolicy
    -- ** Request constructor
    , updateAssumeRolePolicy
    -- ** Request lenses
    , uarprqRoleName
    , uarprqPolicyDocument

    -- * Response
    , UpdateAssumeRolePolicyResponse
    -- ** Response constructor
    , updateAssumeRolePolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAssumeRolePolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarprqRoleName'
--
-- * 'uarprqPolicyDocument'
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy'
    { _uarprqRoleName       :: !Text
    , _uarprqPolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAssumeRolePolicy' smart constructor.
updateAssumeRolePolicy :: Text -> Text -> UpdateAssumeRolePolicy
updateAssumeRolePolicy pRoleName_ pPolicyDocument_ =
    UpdateAssumeRolePolicy'
    { _uarprqRoleName = pRoleName_
    , _uarprqPolicyDocument = pPolicyDocument_
    }

-- | The name of the role to update.
uarprqRoleName :: Lens' UpdateAssumeRolePolicy Text
uarprqRoleName = lens _uarprqRoleName (\ s a -> s{_uarprqRoleName = a});

-- | The policy that grants an entity permission to assume the role.
uarprqPolicyDocument :: Lens' UpdateAssumeRolePolicy Text
uarprqPolicyDocument = lens _uarprqPolicyDocument (\ s a -> s{_uarprqPolicyDocument = a});

instance AWSRequest UpdateAssumeRolePolicy where
        type Sv UpdateAssumeRolePolicy = IAM
        type Rs UpdateAssumeRolePolicy =
             UpdateAssumeRolePolicyResponse
        request = post
        response
          = receiveNull UpdateAssumeRolePolicyResponse'

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
               "RoleName" =: _uarprqRoleName,
               "PolicyDocument" =: _uarprqPolicyDocument]

-- | /See:/ 'updateAssumeRolePolicyResponse' smart constructor.
data UpdateAssumeRolePolicyResponse =
    UpdateAssumeRolePolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAssumeRolePolicyResponse' smart constructor.
updateAssumeRolePolicyResponse :: UpdateAssumeRolePolicyResponse
updateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse'
