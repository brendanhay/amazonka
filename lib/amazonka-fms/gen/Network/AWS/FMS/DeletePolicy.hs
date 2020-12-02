{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager policy.
module Network.AWS.FMS.DeletePolicy
  ( -- * Creating a Request
    deletePolicy,
    DeletePolicy,

    -- * Request Lenses
    dpDeleteAllPolicyResources,
    dpPolicyId,

    -- * Destructuring the Response
    deletePolicyResponse,
    DeletePolicyResponse,
  )
where

import Network.AWS.FMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { _dpDeleteAllPolicyResources ::
      !(Maybe Bool),
    _dpPolicyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpDeleteAllPolicyResources' - If @True@ , the request performs cleanup according to the policy type.  For AWS WAF and Shield Advanced policies, the cleanup does the following:     * Deletes rule groups created by AWS Firewall Manager     * Removes web ACLs from in-scope resources     * Deletes web ACLs that contain no rules or rule groups For security group policies, the cleanup does the following for each security group in the policy:     * Disassociates the security group from in-scope resources      * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope.
--
-- * 'dpPolicyId' - The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
deletePolicy ::
  -- | 'dpPolicyId'
  Text ->
  DeletePolicy
deletePolicy pPolicyId_ =
  DeletePolicy'
    { _dpDeleteAllPolicyResources = Nothing,
      _dpPolicyId = pPolicyId_
    }

-- | If @True@ , the request performs cleanup according to the policy type.  For AWS WAF and Shield Advanced policies, the cleanup does the following:     * Deletes rule groups created by AWS Firewall Manager     * Removes web ACLs from in-scope resources     * Deletes web ACLs that contain no rules or rule groups For security group policies, the cleanup does the following for each security group in the policy:     * Disassociates the security group from in-scope resources      * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope.
dpDeleteAllPolicyResources :: Lens' DeletePolicy (Maybe Bool)
dpDeleteAllPolicyResources = lens _dpDeleteAllPolicyResources (\s a -> s {_dpDeleteAllPolicyResources = a})

-- | The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
dpPolicyId :: Lens' DeletePolicy Text
dpPolicyId = lens _dpPolicyId (\s a -> s {_dpPolicyId = a})

instance AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = postJSON fms
  response = receiveNull DeletePolicyResponse'

instance Hashable DeletePolicy

instance NFData DeletePolicy

instance ToHeaders DeletePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSFMS_20180101.DeletePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    object
      ( catMaybes
          [ ("DeleteAllPolicyResources" .=) <$> _dpDeleteAllPolicyResources,
            Just ("PolicyId" .= _dpPolicyId)
          ]
      )

instance ToPath DeletePolicy where
  toPath = const "/"

instance ToQuery DeletePolicy where
  toQuery = const mempty

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
deletePolicyResponse ::
  DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'

instance NFData DeletePolicyResponse
