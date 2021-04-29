{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.GetLayerVersionPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the permission policy for a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
-- For more information, see AddLayerVersionPermission.
module Network.AWS.Lambda.GetLayerVersionPolicy
  ( -- * Creating a Request
    GetLayerVersionPolicy (..),
    newGetLayerVersionPolicy,

    -- * Request Lenses
    getLayerVersionPolicy_layerName,
    getLayerVersionPolicy_versionNumber,

    -- * Destructuring the Response
    GetLayerVersionPolicyResponse (..),
    newGetLayerVersionPolicyResponse,

    -- * Response Lenses
    getLayerVersionPolicyResponse_revisionId,
    getLayerVersionPolicyResponse_policy,
    getLayerVersionPolicyResponse_httpStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLayerVersionPolicy' smart constructor.
data GetLayerVersionPolicy = GetLayerVersionPolicy'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerName', 'getLayerVersionPolicy_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'getLayerVersionPolicy_versionNumber' - The version number.
newGetLayerVersionPolicy ::
  -- | 'layerName'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Integer ->
  GetLayerVersionPolicy
newGetLayerVersionPolicy pLayerName_ pVersionNumber_ =
  GetLayerVersionPolicy'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
getLayerVersionPolicy_layerName :: Lens.Lens' GetLayerVersionPolicy Prelude.Text
getLayerVersionPolicy_layerName = Lens.lens (\GetLayerVersionPolicy' {layerName} -> layerName) (\s@GetLayerVersionPolicy' {} a -> s {layerName = a} :: GetLayerVersionPolicy)

-- | The version number.
getLayerVersionPolicy_versionNumber :: Lens.Lens' GetLayerVersionPolicy Prelude.Integer
getLayerVersionPolicy_versionNumber = Lens.lens (\GetLayerVersionPolicy' {versionNumber} -> versionNumber) (\s@GetLayerVersionPolicy' {} a -> s {versionNumber = a} :: GetLayerVersionPolicy)

instance Prelude.AWSRequest GetLayerVersionPolicy where
  type
    Rs GetLayerVersionPolicy =
      GetLayerVersionPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLayerVersionPolicyResponse'
            Prelude.<$> (x Prelude..?> "RevisionId")
            Prelude.<*> (x Prelude..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLayerVersionPolicy

instance Prelude.NFData GetLayerVersionPolicy

instance Prelude.ToHeaders GetLayerVersionPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetLayerVersionPolicy where
  toPath GetLayerVersionPolicy' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Prelude.toBS layerName,
        "/versions/",
        Prelude.toBS versionNumber,
        "/policy"
      ]

instance Prelude.ToQuery GetLayerVersionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLayerVersionPolicyResponse' smart constructor.
data GetLayerVersionPolicyResponse = GetLayerVersionPolicyResponse'
  { -- | A unique identifier for the current revision of the policy.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The policy document.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'getLayerVersionPolicyResponse_revisionId' - A unique identifier for the current revision of the policy.
--
-- 'policy', 'getLayerVersionPolicyResponse_policy' - The policy document.
--
-- 'httpStatus', 'getLayerVersionPolicyResponse_httpStatus' - The response's http status code.
newGetLayerVersionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLayerVersionPolicyResponse
newGetLayerVersionPolicyResponse pHttpStatus_ =
  GetLayerVersionPolicyResponse'
    { revisionId =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the current revision of the policy.
getLayerVersionPolicyResponse_revisionId :: Lens.Lens' GetLayerVersionPolicyResponse (Prelude.Maybe Prelude.Text)
getLayerVersionPolicyResponse_revisionId = Lens.lens (\GetLayerVersionPolicyResponse' {revisionId} -> revisionId) (\s@GetLayerVersionPolicyResponse' {} a -> s {revisionId = a} :: GetLayerVersionPolicyResponse)

-- | The policy document.
getLayerVersionPolicyResponse_policy :: Lens.Lens' GetLayerVersionPolicyResponse (Prelude.Maybe Prelude.Text)
getLayerVersionPolicyResponse_policy = Lens.lens (\GetLayerVersionPolicyResponse' {policy} -> policy) (\s@GetLayerVersionPolicyResponse' {} a -> s {policy = a} :: GetLayerVersionPolicyResponse)

-- | The response's http status code.
getLayerVersionPolicyResponse_httpStatus :: Lens.Lens' GetLayerVersionPolicyResponse Prelude.Int
getLayerVersionPolicyResponse_httpStatus = Lens.lens (\GetLayerVersionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetLayerVersionPolicyResponse' {} a -> s {httpStatus = a} :: GetLayerVersionPolicyResponse)

instance Prelude.NFData GetLayerVersionPolicyResponse
