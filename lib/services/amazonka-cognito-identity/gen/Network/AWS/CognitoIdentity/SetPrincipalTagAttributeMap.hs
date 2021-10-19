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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetPrincipalTagAttributeMap' smart constructor.
data SetPrincipalTagAttributeMap = SetPrincipalTagAttributeMap'
  { -- | You can use this operation to add principal tags.
    principalTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | You can use this operation to use default (username and clientID)
    -- attribute mappings.
    useDefaults :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Identity Pool you want to set attribute mappings for.
    identityPoolId :: Prelude.Text,
    -- | The provider name you want to use for attribute mappings.
    identityProviderName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'identityProviderName'
  Prelude.Text ->
  SetPrincipalTagAttributeMap
newSetPrincipalTagAttributeMap
  pIdentityPoolId_
  pIdentityProviderName_ =
    SetPrincipalTagAttributeMap'
      { principalTags =
          Prelude.Nothing,
        useDefaults = Prelude.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityProviderName = pIdentityProviderName_
      }

-- | You can use this operation to add principal tags.
setPrincipalTagAttributeMap_principalTags :: Lens.Lens' SetPrincipalTagAttributeMap (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
setPrincipalTagAttributeMap_principalTags = Lens.lens (\SetPrincipalTagAttributeMap' {principalTags} -> principalTags) (\s@SetPrincipalTagAttributeMap' {} a -> s {principalTags = a} :: SetPrincipalTagAttributeMap) Prelude.. Lens.mapping Lens.coerced

-- | You can use this operation to use default (username and clientID)
-- attribute mappings.
setPrincipalTagAttributeMap_useDefaults :: Lens.Lens' SetPrincipalTagAttributeMap (Prelude.Maybe Prelude.Bool)
setPrincipalTagAttributeMap_useDefaults = Lens.lens (\SetPrincipalTagAttributeMap' {useDefaults} -> useDefaults) (\s@SetPrincipalTagAttributeMap' {} a -> s {useDefaults = a} :: SetPrincipalTagAttributeMap)

-- | The ID of the Identity Pool you want to set attribute mappings for.
setPrincipalTagAttributeMap_identityPoolId :: Lens.Lens' SetPrincipalTagAttributeMap Prelude.Text
setPrincipalTagAttributeMap_identityPoolId = Lens.lens (\SetPrincipalTagAttributeMap' {identityPoolId} -> identityPoolId) (\s@SetPrincipalTagAttributeMap' {} a -> s {identityPoolId = a} :: SetPrincipalTagAttributeMap)

-- | The provider name you want to use for attribute mappings.
setPrincipalTagAttributeMap_identityProviderName :: Lens.Lens' SetPrincipalTagAttributeMap Prelude.Text
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
            Prelude.<$> (x Core..?> "IdentityPoolId")
            Prelude.<*> (x Core..?> "IdentityProviderName")
            Prelude.<*> (x Core..?> "PrincipalTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "UseDefaults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetPrincipalTagAttributeMap

instance Prelude.NFData SetPrincipalTagAttributeMap

instance Core.ToHeaders SetPrincipalTagAttributeMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.SetPrincipalTagAttributeMap" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetPrincipalTagAttributeMap where
  toJSON SetPrincipalTagAttributeMap' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PrincipalTags" Core..=) Prelude.<$> principalTags,
            ("UseDefaults" Core..=) Prelude.<$> useDefaults,
            Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId),
            Prelude.Just
              ( "IdentityProviderName"
                  Core..= identityProviderName
              )
          ]
      )

instance Core.ToPath SetPrincipalTagAttributeMap where
  toPath = Prelude.const "/"

instance Core.ToQuery SetPrincipalTagAttributeMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetPrincipalTagAttributeMapResponse' smart constructor.
data SetPrincipalTagAttributeMapResponse = SetPrincipalTagAttributeMapResponse'
  { -- | The ID of the Identity Pool you want to set attribute mappings for.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The provider name you want to use for attribute mappings.
    identityProviderName :: Prelude.Maybe Prelude.Text,
    -- | You can use this operation to add principal tags. The
    -- @PrincipalTags@operation enables you to reference user attributes in
    -- your IAM permissions policy.
    principalTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | You can use this operation to select default (username and clientID)
    -- attribute mappings.
    useDefaults :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SetPrincipalTagAttributeMapResponse
newSetPrincipalTagAttributeMapResponse pHttpStatus_ =
  SetPrincipalTagAttributeMapResponse'
    { identityPoolId =
        Prelude.Nothing,
      identityProviderName = Prelude.Nothing,
      principalTags = Prelude.Nothing,
      useDefaults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Identity Pool you want to set attribute mappings for.
setPrincipalTagAttributeMapResponse_identityPoolId :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Prelude.Maybe Prelude.Text)
setPrincipalTagAttributeMapResponse_identityPoolId = Lens.lens (\SetPrincipalTagAttributeMapResponse' {identityPoolId} -> identityPoolId) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {identityPoolId = a} :: SetPrincipalTagAttributeMapResponse)

-- | The provider name you want to use for attribute mappings.
setPrincipalTagAttributeMapResponse_identityProviderName :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Prelude.Maybe Prelude.Text)
setPrincipalTagAttributeMapResponse_identityProviderName = Lens.lens (\SetPrincipalTagAttributeMapResponse' {identityProviderName} -> identityProviderName) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {identityProviderName = a} :: SetPrincipalTagAttributeMapResponse)

-- | You can use this operation to add principal tags. The
-- @PrincipalTags@operation enables you to reference user attributes in
-- your IAM permissions policy.
setPrincipalTagAttributeMapResponse_principalTags :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
setPrincipalTagAttributeMapResponse_principalTags = Lens.lens (\SetPrincipalTagAttributeMapResponse' {principalTags} -> principalTags) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {principalTags = a} :: SetPrincipalTagAttributeMapResponse) Prelude.. Lens.mapping Lens.coerced

-- | You can use this operation to select default (username and clientID)
-- attribute mappings.
setPrincipalTagAttributeMapResponse_useDefaults :: Lens.Lens' SetPrincipalTagAttributeMapResponse (Prelude.Maybe Prelude.Bool)
setPrincipalTagAttributeMapResponse_useDefaults = Lens.lens (\SetPrincipalTagAttributeMapResponse' {useDefaults} -> useDefaults) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {useDefaults = a} :: SetPrincipalTagAttributeMapResponse)

-- | The response's http status code.
setPrincipalTagAttributeMapResponse_httpStatus :: Lens.Lens' SetPrincipalTagAttributeMapResponse Prelude.Int
setPrincipalTagAttributeMapResponse_httpStatus = Lens.lens (\SetPrincipalTagAttributeMapResponse' {httpStatus} -> httpStatus) (\s@SetPrincipalTagAttributeMapResponse' {} a -> s {httpStatus = a} :: SetPrincipalTagAttributeMapResponse)

instance
  Prelude.NFData
    SetPrincipalTagAttributeMapResponse
