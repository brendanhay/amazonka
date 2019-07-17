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
-- Module      : Network.AWS.IAM.PutRolePermissionsBoundary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM role's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a role. Use the boundary to control the maximum permissions that the role can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the role.
--
--
-- You cannot set the boundary for a service-linked role.
--
-- /Important:/ Policies used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the role. To learn how the effective permissions for a role are evaluated, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide.
--
module Network.AWS.IAM.PutRolePermissionsBoundary
    (
    -- * Creating a Request
      putRolePermissionsBoundary
    , PutRolePermissionsBoundary
    -- * Request Lenses
    , prpbRoleName
    , prpbPermissionsBoundary

    -- * Destructuring the Response
    , putRolePermissionsBoundaryResponse
    , PutRolePermissionsBoundaryResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRolePermissionsBoundary' smart constructor.
data PutRolePermissionsBoundary = PutRolePermissionsBoundary'
  { _prpbRoleName            :: !Text
  , _prpbPermissionsBoundary :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRolePermissionsBoundary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prpbRoleName' - The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
--
-- * 'prpbPermissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the role.
putRolePermissionsBoundary
    :: Text -- ^ 'prpbRoleName'
    -> Text -- ^ 'prpbPermissionsBoundary'
    -> PutRolePermissionsBoundary
putRolePermissionsBoundary pRoleName_ pPermissionsBoundary_ =
  PutRolePermissionsBoundary'
    { _prpbRoleName = pRoleName_
    , _prpbPermissionsBoundary = pPermissionsBoundary_
    }


-- | The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
prpbRoleName :: Lens' PutRolePermissionsBoundary Text
prpbRoleName = lens _prpbRoleName (\ s a -> s{_prpbRoleName = a})

-- | The ARN of the policy that is used to set the permissions boundary for the role.
prpbPermissionsBoundary :: Lens' PutRolePermissionsBoundary Text
prpbPermissionsBoundary = lens _prpbPermissionsBoundary (\ s a -> s{_prpbPermissionsBoundary = a})

instance AWSRequest PutRolePermissionsBoundary where
        type Rs PutRolePermissionsBoundary =
             PutRolePermissionsBoundaryResponse
        request = postQuery iam
        response
          = receiveNull PutRolePermissionsBoundaryResponse'

instance Hashable PutRolePermissionsBoundary where

instance NFData PutRolePermissionsBoundary where

instance ToHeaders PutRolePermissionsBoundary where
        toHeaders = const mempty

instance ToPath PutRolePermissionsBoundary where
        toPath = const "/"

instance ToQuery PutRolePermissionsBoundary where
        toQuery PutRolePermissionsBoundary'{..}
          = mconcat
              ["Action" =:
                 ("PutRolePermissionsBoundary" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _prpbRoleName,
               "PermissionsBoundary" =: _prpbPermissionsBoundary]

-- | /See:/ 'putRolePermissionsBoundaryResponse' smart constructor.
data PutRolePermissionsBoundaryResponse =
  PutRolePermissionsBoundaryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRolePermissionsBoundaryResponse' with the minimum fields required to make a request.
--
putRolePermissionsBoundaryResponse
    :: PutRolePermissionsBoundaryResponse
putRolePermissionsBoundaryResponse = PutRolePermissionsBoundaryResponse'


instance NFData PutRolePermissionsBoundaryResponse
         where
