{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetPrincipalTagAttributeMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use @GetPrincipalTagAttributeMap@ to list all mappings between
-- @PrincipalTags@ and user attributes.
module Network.AWS.CognitoIdentity.GetPrincipalTagAttributeMap
  ( -- * Creating a Request
    GetPrincipalTagAttributeMap (..),
    newGetPrincipalTagAttributeMap,

    -- * Request Lenses
    getPrincipalTagAttributeMap_identityPoolId,
    getPrincipalTagAttributeMap_identityProviderName,

    -- * Destructuring the Response
    GetPrincipalTagAttributeMapResponse (..),
    newGetPrincipalTagAttributeMapResponse,

    -- * Response Lenses
    getPrincipalTagAttributeMapResponse_identityPoolId,
    getPrincipalTagAttributeMapResponse_identityProviderName,
    getPrincipalTagAttributeMapResponse_principalTags,
    getPrincipalTagAttributeMapResponse_useDefaults,
    getPrincipalTagAttributeMapResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPrincipalTagAttributeMap' smart constructor.
data GetPrincipalTagAttributeMap = GetPrincipalTagAttributeMap'
  { -- | You can use this operation to get the ID of the Identity Pool you setup
    -- attribute mappings for.
    identityPoolId :: Core.Text,
    -- | You can use this operation to get the provider name.
    identityProviderName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPrincipalTagAttributeMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getPrincipalTagAttributeMap_identityPoolId' - You can use this operation to get the ID of the Identity Pool you setup
-- attribute mappings for.
--
-- 'identityProviderName', 'getPrincipalTagAttributeMap_identityProviderName' - You can use this operation to get the provider name.
newGetPrincipalTagAttributeMap ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityProviderName'
  Core.Text ->
  GetPrincipalTagAttributeMap
newGetPrincipalTagAttributeMap
  pIdentityPoolId_
  pIdentityProviderName_ =
    GetPrincipalTagAttributeMap'
      { identityPoolId =
          pIdentityPoolId_,
        identityProviderName = pIdentityProviderName_
      }

-- | You can use this operation to get the ID of the Identity Pool you setup
-- attribute mappings for.
getPrincipalTagAttributeMap_identityPoolId :: Lens.Lens' GetPrincipalTagAttributeMap Core.Text
getPrincipalTagAttributeMap_identityPoolId = Lens.lens (\GetPrincipalTagAttributeMap' {identityPoolId} -> identityPoolId) (\s@GetPrincipalTagAttributeMap' {} a -> s {identityPoolId = a} :: GetPrincipalTagAttributeMap)

-- | You can use this operation to get the provider name.
getPrincipalTagAttributeMap_identityProviderName :: Lens.Lens' GetPrincipalTagAttributeMap Core.Text
getPrincipalTagAttributeMap_identityProviderName = Lens.lens (\GetPrincipalTagAttributeMap' {identityProviderName} -> identityProviderName) (\s@GetPrincipalTagAttributeMap' {} a -> s {identityProviderName = a} :: GetPrincipalTagAttributeMap)

instance Core.AWSRequest GetPrincipalTagAttributeMap where
  type
    AWSResponse GetPrincipalTagAttributeMap =
      GetPrincipalTagAttributeMapResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPrincipalTagAttributeMapResponse'
            Core.<$> (x Core..?> "IdentityPoolId")
            Core.<*> (x Core..?> "IdentityProviderName")
            Core.<*> (x Core..?> "PrincipalTags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "UseDefaults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPrincipalTagAttributeMap

instance Core.NFData GetPrincipalTagAttributeMap

instance Core.ToHeaders GetPrincipalTagAttributeMap where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.GetPrincipalTagAttributeMap" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPrincipalTagAttributeMap where
  toJSON GetPrincipalTagAttributeMap' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just
              ( "IdentityProviderName"
                  Core..= identityProviderName
              )
          ]
      )

instance Core.ToPath GetPrincipalTagAttributeMap where
  toPath = Core.const "/"

instance Core.ToQuery GetPrincipalTagAttributeMap where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPrincipalTagAttributeMapResponse' smart constructor.
data GetPrincipalTagAttributeMapResponse = GetPrincipalTagAttributeMapResponse'
  { -- | You can use this operation to get the ID of the Identity Pool you setup
    -- attribute mappings for.
    identityPoolId :: Core.Maybe Core.Text,
    -- | You can use this operation to get the provider name.
    identityProviderName :: Core.Maybe Core.Text,
    -- | You can use this operation to add principal tags. The
    -- @PrincipalTags@operation enables you to reference user attributes in
    -- your IAM permissions policy.
    principalTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | You can use this operation to list
    useDefaults :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPrincipalTagAttributeMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getPrincipalTagAttributeMapResponse_identityPoolId' - You can use this operation to get the ID of the Identity Pool you setup
-- attribute mappings for.
--
-- 'identityProviderName', 'getPrincipalTagAttributeMapResponse_identityProviderName' - You can use this operation to get the provider name.
--
-- 'principalTags', 'getPrincipalTagAttributeMapResponse_principalTags' - You can use this operation to add principal tags. The
-- @PrincipalTags@operation enables you to reference user attributes in
-- your IAM permissions policy.
--
-- 'useDefaults', 'getPrincipalTagAttributeMapResponse_useDefaults' - You can use this operation to list
--
-- 'httpStatus', 'getPrincipalTagAttributeMapResponse_httpStatus' - The response's http status code.
newGetPrincipalTagAttributeMapResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPrincipalTagAttributeMapResponse
newGetPrincipalTagAttributeMapResponse pHttpStatus_ =
  GetPrincipalTagAttributeMapResponse'
    { identityPoolId =
        Core.Nothing,
      identityProviderName = Core.Nothing,
      principalTags = Core.Nothing,
      useDefaults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | You can use this operation to get the ID of the Identity Pool you setup
-- attribute mappings for.
getPrincipalTagAttributeMapResponse_identityPoolId :: Lens.Lens' GetPrincipalTagAttributeMapResponse (Core.Maybe Core.Text)
getPrincipalTagAttributeMapResponse_identityPoolId = Lens.lens (\GetPrincipalTagAttributeMapResponse' {identityPoolId} -> identityPoolId) (\s@GetPrincipalTagAttributeMapResponse' {} a -> s {identityPoolId = a} :: GetPrincipalTagAttributeMapResponse)

-- | You can use this operation to get the provider name.
getPrincipalTagAttributeMapResponse_identityProviderName :: Lens.Lens' GetPrincipalTagAttributeMapResponse (Core.Maybe Core.Text)
getPrincipalTagAttributeMapResponse_identityProviderName = Lens.lens (\GetPrincipalTagAttributeMapResponse' {identityProviderName} -> identityProviderName) (\s@GetPrincipalTagAttributeMapResponse' {} a -> s {identityProviderName = a} :: GetPrincipalTagAttributeMapResponse)

-- | You can use this operation to add principal tags. The
-- @PrincipalTags@operation enables you to reference user attributes in
-- your IAM permissions policy.
getPrincipalTagAttributeMapResponse_principalTags :: Lens.Lens' GetPrincipalTagAttributeMapResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getPrincipalTagAttributeMapResponse_principalTags = Lens.lens (\GetPrincipalTagAttributeMapResponse' {principalTags} -> principalTags) (\s@GetPrincipalTagAttributeMapResponse' {} a -> s {principalTags = a} :: GetPrincipalTagAttributeMapResponse) Core.. Lens.mapping Lens._Coerce

-- | You can use this operation to list
getPrincipalTagAttributeMapResponse_useDefaults :: Lens.Lens' GetPrincipalTagAttributeMapResponse (Core.Maybe Core.Bool)
getPrincipalTagAttributeMapResponse_useDefaults = Lens.lens (\GetPrincipalTagAttributeMapResponse' {useDefaults} -> useDefaults) (\s@GetPrincipalTagAttributeMapResponse' {} a -> s {useDefaults = a} :: GetPrincipalTagAttributeMapResponse)

-- | The response's http status code.
getPrincipalTagAttributeMapResponse_httpStatus :: Lens.Lens' GetPrincipalTagAttributeMapResponse Core.Int
getPrincipalTagAttributeMapResponse_httpStatus = Lens.lens (\GetPrincipalTagAttributeMapResponse' {httpStatus} -> httpStatus) (\s@GetPrincipalTagAttributeMapResponse' {} a -> s {httpStatus = a} :: GetPrincipalTagAttributeMapResponse)

instance
  Core.NFData
    GetPrincipalTagAttributeMapResponse
