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
-- Module      : Network.AWS.CloudFront.DeleteOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an origin request policy.
--
--
-- You cannot delete an origin request policy if it’s attached to any cache behaviors. First update your distributions to remove the origin request policy from all cache behaviors, then delete the origin request policy.
--
-- To delete an origin request policy, you must provide the policy’s identifier and version. To get the identifier, you can use @ListOriginRequestPolicies@ or @GetOriginRequestPolicy@ .
module Network.AWS.CloudFront.DeleteOriginRequestPolicy
  ( -- * Creating a Request
    deleteOriginRequestPolicy,
    DeleteOriginRequestPolicy,

    -- * Request Lenses
    dorpIfMatch,
    dorpId,

    -- * Destructuring the Response
    deleteOriginRequestPolicyResponse,
    DeleteOriginRequestPolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOriginRequestPolicy' smart constructor.
data DeleteOriginRequestPolicy = DeleteOriginRequestPolicy'
  { _dorpIfMatch ::
      !(Maybe Text),
    _dorpId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOriginRequestPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorpIfMatch' - The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
--
-- * 'dorpId' - The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
deleteOriginRequestPolicy ::
  -- | 'dorpId'
  Text ->
  DeleteOriginRequestPolicy
deleteOriginRequestPolicy pId_ =
  DeleteOriginRequestPolicy'
    { _dorpIfMatch = Nothing,
      _dorpId = pId_
    }

-- | The version of the origin request policy that you are deleting. The version is the origin request policy’s @ETag@ value, which you can get using @ListOriginRequestPolicies@ , @GetOriginRequestPolicy@ , or @GetOriginRequestPolicyConfig@ .
dorpIfMatch :: Lens' DeleteOriginRequestPolicy (Maybe Text)
dorpIfMatch = lens _dorpIfMatch (\s a -> s {_dorpIfMatch = a})

-- | The unique identifier for the origin request policy that you are deleting. To get the identifier, you can use @ListOriginRequestPolicies@ .
dorpId :: Lens' DeleteOriginRequestPolicy Text
dorpId = lens _dorpId (\s a -> s {_dorpId = a})

instance AWSRequest DeleteOriginRequestPolicy where
  type
    Rs DeleteOriginRequestPolicy =
      DeleteOriginRequestPolicyResponse
  request = delete cloudFront
  response = receiveNull DeleteOriginRequestPolicyResponse'

instance Hashable DeleteOriginRequestPolicy

instance NFData DeleteOriginRequestPolicy

instance ToHeaders DeleteOriginRequestPolicy where
  toHeaders DeleteOriginRequestPolicy' {..} =
    mconcat ["If-Match" =# _dorpIfMatch]

instance ToPath DeleteOriginRequestPolicy where
  toPath DeleteOriginRequestPolicy' {..} =
    mconcat ["/2020-05-31/origin-request-policy/", toBS _dorpId]

instance ToQuery DeleteOriginRequestPolicy where
  toQuery = const mempty

-- | /See:/ 'deleteOriginRequestPolicyResponse' smart constructor.
data DeleteOriginRequestPolicyResponse = DeleteOriginRequestPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOriginRequestPolicyResponse' with the minimum fields required to make a request.
deleteOriginRequestPolicyResponse ::
  DeleteOriginRequestPolicyResponse
deleteOriginRequestPolicyResponse =
  DeleteOriginRequestPolicyResponse'

instance NFData DeleteOriginRequestPolicyResponse
