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
-- Module      : Network.AWS.IoT.DeletePolicyVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of the specified policy. You cannot delete the default version of a policy using this API. To delete the default version of a policy, use 'DeletePolicy' . To find out which version of a policy is marked as the default version, use ListPolicyVersions.
--
--
module Network.AWS.IoT.DeletePolicyVersion
    (
    -- * Creating a Request
      deletePolicyVersion
    , DeletePolicyVersion
    -- * Request Lenses
    , dpvPolicyName
    , dpvPolicyVersionId

    -- * Destructuring the Response
    , deletePolicyVersionResponse
    , DeletePolicyVersionResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DeletePolicyVersion operation.
--
--
--
-- /See:/ 'deletePolicyVersion' smart constructor.
data DeletePolicyVersion = DeletePolicyVersion'
  { _dpvPolicyName      :: !Text
  , _dpvPolicyVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvPolicyName' - The name of the policy.
--
-- * 'dpvPolicyVersionId' - The policy version ID.
deletePolicyVersion
    :: Text -- ^ 'dpvPolicyName'
    -> Text -- ^ 'dpvPolicyVersionId'
    -> DeletePolicyVersion
deletePolicyVersion pPolicyName_ pPolicyVersionId_ =
  DeletePolicyVersion'
    {_dpvPolicyName = pPolicyName_, _dpvPolicyVersionId = pPolicyVersionId_}


-- | The name of the policy.
dpvPolicyName :: Lens' DeletePolicyVersion Text
dpvPolicyName = lens _dpvPolicyName (\ s a -> s{_dpvPolicyName = a})

-- | The policy version ID.
dpvPolicyVersionId :: Lens' DeletePolicyVersion Text
dpvPolicyVersionId = lens _dpvPolicyVersionId (\ s a -> s{_dpvPolicyVersionId = a})

instance AWSRequest DeletePolicyVersion where
        type Rs DeletePolicyVersion =
             DeletePolicyVersionResponse
        request = delete ioT
        response = receiveNull DeletePolicyVersionResponse'

instance Hashable DeletePolicyVersion where

instance NFData DeletePolicyVersion where

instance ToHeaders DeletePolicyVersion where
        toHeaders = const mempty

instance ToPath DeletePolicyVersion where
        toPath DeletePolicyVersion'{..}
          = mconcat
              ["/policies/", toBS _dpvPolicyName, "/version/",
               toBS _dpvPolicyVersionId]

instance ToQuery DeletePolicyVersion where
        toQuery = const mempty

-- | /See:/ 'deletePolicyVersionResponse' smart constructor.
data DeletePolicyVersionResponse =
  DeletePolicyVersionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePolicyVersionResponse' with the minimum fields required to make a request.
--
deletePolicyVersionResponse
    :: DeletePolicyVersionResponse
deletePolicyVersionResponse = DeletePolicyVersionResponse'


instance NFData DeletePolicyVersionResponse where
