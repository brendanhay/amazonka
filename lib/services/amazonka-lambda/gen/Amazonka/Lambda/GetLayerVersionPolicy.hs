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
-- Module      : Amazonka.Lambda.GetLayerVersionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the permission policy for a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
-- For more information, see AddLayerVersionPermission.
module Amazonka.Lambda.GetLayerVersionPolicy
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
    getLayerVersionPolicyResponse_policy,
    getLayerVersionPolicyResponse_revisionId,
    getLayerVersionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLayerVersionPolicy' smart constructor.
data GetLayerVersionPolicy = GetLayerVersionPolicy'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetLayerVersionPolicy where
  type
    AWSResponse GetLayerVersionPolicy =
      GetLayerVersionPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLayerVersionPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (x Data..?> "RevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLayerVersionPolicy where
  hashWithSalt _salt GetLayerVersionPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData GetLayerVersionPolicy where
  rnf GetLayerVersionPolicy' {..} =
    Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf versionNumber

instance Data.ToHeaders GetLayerVersionPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLayerVersionPolicy where
  toPath GetLayerVersionPolicy' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions/",
        Data.toBS versionNumber,
        "/policy"
      ]

instance Data.ToQuery GetLayerVersionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLayerVersionPolicyResponse' smart constructor.
data GetLayerVersionPolicyResponse = GetLayerVersionPolicyResponse'
  { -- | The policy document.
    policy :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the current revision of the policy.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayerVersionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getLayerVersionPolicyResponse_policy' - The policy document.
--
-- 'revisionId', 'getLayerVersionPolicyResponse_revisionId' - A unique identifier for the current revision of the policy.
--
-- 'httpStatus', 'getLayerVersionPolicyResponse_httpStatus' - The response's http status code.
newGetLayerVersionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLayerVersionPolicyResponse
newGetLayerVersionPolicyResponse pHttpStatus_ =
  GetLayerVersionPolicyResponse'
    { policy =
        Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy document.
getLayerVersionPolicyResponse_policy :: Lens.Lens' GetLayerVersionPolicyResponse (Prelude.Maybe Prelude.Text)
getLayerVersionPolicyResponse_policy = Lens.lens (\GetLayerVersionPolicyResponse' {policy} -> policy) (\s@GetLayerVersionPolicyResponse' {} a -> s {policy = a} :: GetLayerVersionPolicyResponse)

-- | A unique identifier for the current revision of the policy.
getLayerVersionPolicyResponse_revisionId :: Lens.Lens' GetLayerVersionPolicyResponse (Prelude.Maybe Prelude.Text)
getLayerVersionPolicyResponse_revisionId = Lens.lens (\GetLayerVersionPolicyResponse' {revisionId} -> revisionId) (\s@GetLayerVersionPolicyResponse' {} a -> s {revisionId = a} :: GetLayerVersionPolicyResponse)

-- | The response's http status code.
getLayerVersionPolicyResponse_httpStatus :: Lens.Lens' GetLayerVersionPolicyResponse Prelude.Int
getLayerVersionPolicyResponse_httpStatus = Lens.lens (\GetLayerVersionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetLayerVersionPolicyResponse' {} a -> s {httpStatus = a} :: GetLayerVersionPolicyResponse)

instance Prelude.NFData GetLayerVersionPolicyResponse where
  rnf GetLayerVersionPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf httpStatus
