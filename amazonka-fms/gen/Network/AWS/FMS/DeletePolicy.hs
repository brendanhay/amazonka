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
-- Module      : Network.AWS.FMS.DeletePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager policy.
--
--
module Network.AWS.FMS.DeletePolicy
    (
    -- * Creating a Request
      deletePolicy
    , DeletePolicy
    -- * Request Lenses
    , dpDeleteAllPolicyResources
    , dpPolicyId

    -- * Destructuring the Response
    , deletePolicyResponse
    , DeletePolicyResponse
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { _dpDeleteAllPolicyResources :: !(Maybe Bool)
  , _dpPolicyId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpDeleteAllPolicyResources' - If @True@ , the request will also delete all web ACLs in this policy. Associated resources will no longer be protected by web ACLs in this policy.
--
-- * 'dpPolicyId' - The ID of the policy that you want to delete. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
deletePolicy
    :: Text -- ^ 'dpPolicyId'
    -> DeletePolicy
deletePolicy pPolicyId_ =
  DeletePolicy'
    {_dpDeleteAllPolicyResources = Nothing, _dpPolicyId = pPolicyId_}


-- | If @True@ , the request will also delete all web ACLs in this policy. Associated resources will no longer be protected by web ACLs in this policy.
dpDeleteAllPolicyResources :: Lens' DeletePolicy (Maybe Bool)
dpDeleteAllPolicyResources = lens _dpDeleteAllPolicyResources (\ s a -> s{_dpDeleteAllPolicyResources = a})

-- | The ID of the policy that you want to delete. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
dpPolicyId :: Lens' DeletePolicy Text
dpPolicyId = lens _dpPolicyId (\ s a -> s{_dpPolicyId = a})

instance AWSRequest DeletePolicy where
        type Rs DeletePolicy = DeletePolicyResponse
        request = postJSON fms
        response = receiveNull DeletePolicyResponse'

instance Hashable DeletePolicy where

instance NFData DeletePolicy where

instance ToHeaders DeletePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.DeletePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePolicy where
        toJSON DeletePolicy'{..}
          = object
              (catMaybes
                 [("DeleteAllPolicyResources" .=) <$>
                    _dpDeleteAllPolicyResources,
                  Just ("PolicyId" .= _dpPolicyId)])

instance ToPath DeletePolicy where
        toPath = const "/"

instance ToQuery DeletePolicy where
        toQuery = const mempty

-- | /See:/ 'deletePolicyResponse' smart constructor.
data DeletePolicyResponse =
  DeletePolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
--
deletePolicyResponse
    :: DeletePolicyResponse
deletePolicyResponse = DeletePolicyResponse'


instance NFData DeletePolicyResponse where
