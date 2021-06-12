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
-- Module      : Network.AWS.CognitoIdentity.SetPrincipalTagAttributeMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to use default (username and clientID)
-- attribute or custom attribute mappings.
module Network.AWS.CognitoIdentity.SetPrincipalTagAttributeMap
  ( -- * Creating a Request
    SetPrincipalTagAttributeMap (..),
    newSetPrincipalTagAttributeMap,

    -- * Request Lenses
    setPrincipalTagAttributeMap_principalTags,
    setPrincipalTagAttributeMap_useDefaults,
    setPrincipalTagAttributeMap_identityPoolId,
    setPrincipalTagAttributeMap_identityProviderName,

    -- * Destructuring the Response
    SetPrincipalTagAttributeMapResponse (..),
    newSetPrincipalTagAttributeMapResponse,

    -- * Response Lenses
    setPrincipalTagAttributeMapResponse_identityPoolId,
    setPrincipalTagAttributeMapResponse_identityProviderName,
    setPrincipalTagAttributeMapResponse_principalTags,
    setPrincipalTagAttributeMapResponse_useDefaults,
    setPrincipalTagAttributeMapResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetPrincipalTagAttributeMap' smart constructor.
data SetPrincipalTagAttributeMap = SetPrincipalTagAttributeMap'
  { -- | You can use this operation to add principal tags.
    principalTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | You can use this operation to use default (username and clientID)
    -- attribute mappings.
    useDefaults :: Core.Maybe Core.Bool,
    -- | The ID of the Identity Pool you want to set attribute mappings for.
    identityPoolId :: Core.Text,
    -- | The provider name you want to use for attribute mappings.
    identityProviderName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetPrincipalTagAttributeMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalTags', 'setPrincipalTagAttributeMap_principalTags' - You can use this operation to add principal tags.
--
-- 'useDefaults', 'setPrincipalTagAttributeMap_useDefaults' - You can use this operation to use default (username and clientID)
-- attribute mappings.
--
-- 'identityPoolId', 'setPrincipalTagAttributeMap_identityPoolId' - The ID of the Identity Pool you want to set attribute mappings for.
--
-- 'identityProviderName', 'setPrincipalTagAttributeMap_identityProviderName' - The provider name you want to use for attribute mappings.
newSetPrincipalTagAttributeMap ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityProviderName'
  Core.Text ->
  SetPrincipalTagAttributeMap
newSetPrincipalTagAttributeMap
  pIdentityPoolId_
  pIdentityProviderName_ =
    SetPrincipalTagAttributeMap'
      { principalTags =
          Core.Nothing,
        useDefaults = Core.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityProviderName = pIdentityProviderName_
      }

-- | You can use this operation to add principal tags.
setPrincipalTagAttributeMap_principalTags :: Lens.Lens' SetPrincipalTagAttributeMap (Core.Maybe (Core.HashMap Core.Text Core.Text))
setPrincipalTagAttributeMap_principalTags = Lens.lens (\SetPrincipalTagAttributeMap' {principalTags} -> principalTags) (\s@SetPrincipalTagAttributeMap' {} a -> s {principalTags = a} :: SetPrincipalTagAttributeMap) Core.. Lens.mapping Lens._Coerce

-- | You can use this operation to use default (username and clientID)
-- attribute mappings.
setPrincipalTagAttributeMap_useDefaults :: Lens.Lens' SetPrincipalTagAttributeMap (Core.Maybe Core.Bool)
setPrincipalTagAttributeMap_useDefaults = Lens.lens (\SetPrincipalTagAttributeMap' {useDefaults} -> useDefaults) (\s@SetPrincipalTagAttributeMap' {} a -> s {useDefaults = a} :: SetPrincipalTagAttributeMap)

-- | The ID of the Identity Pool you want to set attribute mappings for.
setPrincipalTagAttributeMap_identityPoolId :: Lens.Lens' SetPrincipalTagAttributeMap Core.Text
setPrincipalTagAttributeMap_identityPoolId = Lens.lens (\SetPrincipalTagAttributeMap' {identityPoolId} -> identityPoolId) (\s@SetPrincipalTagAttributeMap' {} a -> s {identityPoolId = a} :: SetPrincipalTagAttributeMap)

-- | The provider name you want to use for attribute mappings.
setPrincipalTagAttributeMap_identityProviderName :: Lens.Lens' SetPrincipalTagAttributeMap Core.Text
setPrincipalTagAttributeMap_identityProviderName = Lens.lens (\SetPrincipalTagAttributeMap' {identityProviderName} -> identityProviderName) (\s@SetPrincipalTagAttributeMap' {} a -> s {identityProviderName = a} :: SetPrincipalTagAttributeMap)

instance Core.AWSRequest SetPrincipalTagAttributeMap where
  type
    AWSResponse SetPrincipalTagAttributeMap =
      SetPrincipalTagAttributeMapResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetPrincipalTagAttributeMapResponse'
            Core.<$> (x Core..?> "IdentityPoolId")
            Core.<*> (x Core..?> "IdentityProviderName")
            Core.<*> (x Core..?> "PrincipalTags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "UseDefaults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetPrincipalTagAttributeMap

instance Core.NFData SetPrincipalTagAttributeMap

instance Core.ToHeaders SetPrincipalTagAttributeMap where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.SetPrincipalTagAttributeMap" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetPrincipalTagAttributeMap where
  toJSON SetPrincipalTagAttributeMap' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PrincipalTags" Core..=) Core.<$> principalTags,
            ("UseDefaults" Core..=) Core.<$> useDefaults,
            Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just
              ( "IdentityProviderName"
                  Core..= identityProviderName
              )
          ]
      )

instance Core.ToPath SetPrincipalTagAttributeMap where
  toPath = Core.const "/"

instance Core.ToQuery SetPrincipalTagAttributeMap where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetPrincipalTagAttributeMapResponse' smart constructor.
data SetPrincipalTagAttributeMapResponse = SetPrincipalTagAttributeMapResponse'
  { -- | The ID of the Identity Pool you want to set attribute mappings for.
    identityPoolId :: Core.Maybe Core.Text,
    -- | The provider name you want to use for attribute mappings.
    identityProviderName :: Core.Maybe Core.Text,
    -- | You can use this operation to add principal tags. The
    -- @PrincipalTags@operation enables you to reference user attributes in
    -- your IAM permissions policy.
    principalTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | You can use this operation to select default (username and clientID)
    -- attribute mappings.
    useDefaults :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetPrincipalTagAttributeMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'setPrincipalTagAttributeMapResponse_identityPoolId' - The ID of the Identity Pool you want to set attribute mappings for.
--
-- 'identityProviderName', 'setPrincipalTagAttributeMapResponse_identityProviderName' - The provider name you want to use for attribute mappings.
--
-- 'principalTags', 'setPrincipalTagAttributeMapResponse_principalTags' - You can use this operation to add principal tags. The
-- @PrincipalTags@operation enables you to reference user attributes in
-- your IAM permissions policy.
--
-- 'useDefaults', 'setPrincipalTagAttributeMapResponse_useDefaults' - You can use this operation to select default (username and clientID)
-- attribute mappings.
--
-- 'httpStatus', 'setPrincipalTagAttributeMapResponse_httpStatus' - The response's http status code.
newSetPrincipalTagAttributeMapResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetPrincipalTagAttributeMapResponse
newSetPrincipalTagAttributeMapResponse pHttpStatus_ =
  SetPrincipalTagAttributeMapResponse'
    { identityPoolId =
        Core.Nothing,
      identityProviderName = Core.Nothing,
      principalTags = Core.Nothing,
      useDefaults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Identity Pool you want to set attribute mappings for.
setPrincipalTagAttributeMapResponse_identityPoolId :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Core.Maybe Core.Text)
setPrincipalTagAttributeMapResponse_identityPoolId = Lens.lens (\SetPrincipalTagAttributeMapResponse' {identityPoolId} -> identityPoolId) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {identityPoolId = a} :: SetPrincipalTagAttributeMapResponse)

-- | The provider name you want to use for attribute mappings.
setPrincipalTagAttributeMapResponse_identityProviderName :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Core.Maybe Core.Text)
setPrincipalTagAttributeMapResponse_identityProviderName = Lens.lens (\SetPrincipalTagAttributeMapResponse' {identityProviderName} -> identityProviderName) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {identityProviderName = a} :: SetPrincipalTagAttributeMapResponse)

-- | You can use this operation to add principal tags. The
-- @PrincipalTags@operation enables you to reference user attributes in
-- your IAM permissions policy.
setPrincipalTagAttributeMapResponse_principalTags :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
setPrincipalTagAttributeMapResponse_principalTags = Lens.lens (\SetPrincipalTagAttributeMapResponse' {principalTags} -> principalTags) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {principalTags = a} :: SetPrincipalTagAttributeMapResponse) Core.. Lens.mapping Lens._Coerce

-- | You can use this operation to select default (username and clientID)
-- attribute mappings.
setPrincipalTagAttributeMapResponse_useDefaults :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Core.Maybe Core.Bool)
setPrincipalTagAttributeMapResponse_useDefaults = Lens.lens (\SetPrincipalTagAttributeMapResponse' {useDefaults} -> useDefaults) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {useDefaults = a} :: SetPrincipalTagAttributeMapResponse)

-- | The response's http status code.
setPrincipalTagAttributeMapResponse_httpStatus :: Lens.Lens' SetPrincipalTagAttributeMapResponse Core.Int
setPrincipalTagAttributeMapResponse_httpStatus = Lens.lens (\SetPrincipalTagAttributeMapResponse' {httpStatus} -> httpStatus) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {httpStatus = a} :: SetPrincipalTagAttributeMapResponse)

instance
  Core.NFData
    SetPrincipalTagAttributeMapResponse
