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
-- Module      : Amazonka.CloudFront.UpdateContinuousDeploymentPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a continuous deployment policy. You can update a continuous
-- deployment policy to enable or disable it, to change the percentage of
-- traffic that it sends to the staging distribution, or to change the
-- staging distribution that it sends traffic to.
--
-- When you update a continuous deployment policy configuration, all the
-- fields are updated with the values that are provided in the request. You
-- cannot update some fields independent of others. To update a continuous
-- deployment policy configuration:
--
-- 1.  Use @GetContinuousDeploymentPolicyConfig@ to get the current
--     configuration.
--
-- 2.  Locally modify the fields in the continuous deployment policy
--     configuration that you want to update.
--
-- 3.  Use @UpdateContinuousDeploymentPolicy@, providing the entire
--     continuous deployment policy configuration, including the fields
--     that you modified and those that you didn\'t.
module Amazonka.CloudFront.UpdateContinuousDeploymentPolicy
  ( -- * Creating a Request
    UpdateContinuousDeploymentPolicy (..),
    newUpdateContinuousDeploymentPolicy,

    -- * Request Lenses
    updateContinuousDeploymentPolicy_ifMatch,
    updateContinuousDeploymentPolicy_continuousDeploymentPolicyConfig,
    updateContinuousDeploymentPolicy_id,

    -- * Destructuring the Response
    UpdateContinuousDeploymentPolicyResponse (..),
    newUpdateContinuousDeploymentPolicyResponse,

    -- * Response Lenses
    updateContinuousDeploymentPolicyResponse_continuousDeploymentPolicy,
    updateContinuousDeploymentPolicyResponse_eTag,
    updateContinuousDeploymentPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContinuousDeploymentPolicy' smart constructor.
data UpdateContinuousDeploymentPolicy = UpdateContinuousDeploymentPolicy'
  { -- | The current version (@ETag@ value) of the continuous deployment policy
    -- that you are updating.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The continuous deployment policy configuration.
    continuousDeploymentPolicyConfig :: ContinuousDeploymentPolicyConfig,
    -- | The identifier of the continuous deployment policy that you are
    -- updating.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContinuousDeploymentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateContinuousDeploymentPolicy_ifMatch' - The current version (@ETag@ value) of the continuous deployment policy
-- that you are updating.
--
-- 'continuousDeploymentPolicyConfig', 'updateContinuousDeploymentPolicy_continuousDeploymentPolicyConfig' - The continuous deployment policy configuration.
--
-- 'id', 'updateContinuousDeploymentPolicy_id' - The identifier of the continuous deployment policy that you are
-- updating.
newUpdateContinuousDeploymentPolicy ::
  -- | 'continuousDeploymentPolicyConfig'
  ContinuousDeploymentPolicyConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateContinuousDeploymentPolicy
newUpdateContinuousDeploymentPolicy
  pContinuousDeploymentPolicyConfig_
  pId_ =
    UpdateContinuousDeploymentPolicy'
      { ifMatch =
          Prelude.Nothing,
        continuousDeploymentPolicyConfig =
          pContinuousDeploymentPolicyConfig_,
        id = pId_
      }

-- | The current version (@ETag@ value) of the continuous deployment policy
-- that you are updating.
updateContinuousDeploymentPolicy_ifMatch :: Lens.Lens' UpdateContinuousDeploymentPolicy (Prelude.Maybe Prelude.Text)
updateContinuousDeploymentPolicy_ifMatch = Lens.lens (\UpdateContinuousDeploymentPolicy' {ifMatch} -> ifMatch) (\s@UpdateContinuousDeploymentPolicy' {} a -> s {ifMatch = a} :: UpdateContinuousDeploymentPolicy)

-- | The continuous deployment policy configuration.
updateContinuousDeploymentPolicy_continuousDeploymentPolicyConfig :: Lens.Lens' UpdateContinuousDeploymentPolicy ContinuousDeploymentPolicyConfig
updateContinuousDeploymentPolicy_continuousDeploymentPolicyConfig = Lens.lens (\UpdateContinuousDeploymentPolicy' {continuousDeploymentPolicyConfig} -> continuousDeploymentPolicyConfig) (\s@UpdateContinuousDeploymentPolicy' {} a -> s {continuousDeploymentPolicyConfig = a} :: UpdateContinuousDeploymentPolicy)

-- | The identifier of the continuous deployment policy that you are
-- updating.
updateContinuousDeploymentPolicy_id :: Lens.Lens' UpdateContinuousDeploymentPolicy Prelude.Text
updateContinuousDeploymentPolicy_id = Lens.lens (\UpdateContinuousDeploymentPolicy' {id} -> id) (\s@UpdateContinuousDeploymentPolicy' {} a -> s {id = a} :: UpdateContinuousDeploymentPolicy)

instance
  Core.AWSRequest
    UpdateContinuousDeploymentPolicy
  where
  type
    AWSResponse UpdateContinuousDeploymentPolicy =
      UpdateContinuousDeploymentPolicyResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateContinuousDeploymentPolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateContinuousDeploymentPolicy
  where
  hashWithSalt
    _salt
    UpdateContinuousDeploymentPolicy' {..} =
      _salt `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` continuousDeploymentPolicyConfig
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateContinuousDeploymentPolicy
  where
  rnf UpdateContinuousDeploymentPolicy' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf continuousDeploymentPolicyConfig
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToElement
    UpdateContinuousDeploymentPolicy
  where
  toElement UpdateContinuousDeploymentPolicy' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ContinuousDeploymentPolicyConfig"
      continuousDeploymentPolicyConfig

instance
  Data.ToHeaders
    UpdateContinuousDeploymentPolicy
  where
  toHeaders UpdateContinuousDeploymentPolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateContinuousDeploymentPolicy where
  toPath UpdateContinuousDeploymentPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/continuous-deployment-policy/",
        Data.toBS id
      ]

instance
  Data.ToQuery
    UpdateContinuousDeploymentPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContinuousDeploymentPolicyResponse' smart constructor.
data UpdateContinuousDeploymentPolicyResponse = UpdateContinuousDeploymentPolicyResponse'
  { -- | A continuous deployment policy.
    continuousDeploymentPolicy :: Prelude.Maybe ContinuousDeploymentPolicy,
    -- | The version identifier for the current version of the continuous
    -- deployment policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContinuousDeploymentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicy', 'updateContinuousDeploymentPolicyResponse_continuousDeploymentPolicy' - A continuous deployment policy.
--
-- 'eTag', 'updateContinuousDeploymentPolicyResponse_eTag' - The version identifier for the current version of the continuous
-- deployment policy.
--
-- 'httpStatus', 'updateContinuousDeploymentPolicyResponse_httpStatus' - The response's http status code.
newUpdateContinuousDeploymentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContinuousDeploymentPolicyResponse
newUpdateContinuousDeploymentPolicyResponse
  pHttpStatus_ =
    UpdateContinuousDeploymentPolicyResponse'
      { continuousDeploymentPolicy =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A continuous deployment policy.
updateContinuousDeploymentPolicyResponse_continuousDeploymentPolicy :: Lens.Lens' UpdateContinuousDeploymentPolicyResponse (Prelude.Maybe ContinuousDeploymentPolicy)
updateContinuousDeploymentPolicyResponse_continuousDeploymentPolicy = Lens.lens (\UpdateContinuousDeploymentPolicyResponse' {continuousDeploymentPolicy} -> continuousDeploymentPolicy) (\s@UpdateContinuousDeploymentPolicyResponse' {} a -> s {continuousDeploymentPolicy = a} :: UpdateContinuousDeploymentPolicyResponse)

-- | The version identifier for the current version of the continuous
-- deployment policy.
updateContinuousDeploymentPolicyResponse_eTag :: Lens.Lens' UpdateContinuousDeploymentPolicyResponse (Prelude.Maybe Prelude.Text)
updateContinuousDeploymentPolicyResponse_eTag = Lens.lens (\UpdateContinuousDeploymentPolicyResponse' {eTag} -> eTag) (\s@UpdateContinuousDeploymentPolicyResponse' {} a -> s {eTag = a} :: UpdateContinuousDeploymentPolicyResponse)

-- | The response's http status code.
updateContinuousDeploymentPolicyResponse_httpStatus :: Lens.Lens' UpdateContinuousDeploymentPolicyResponse Prelude.Int
updateContinuousDeploymentPolicyResponse_httpStatus = Lens.lens (\UpdateContinuousDeploymentPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateContinuousDeploymentPolicyResponse' {} a -> s {httpStatus = a} :: UpdateContinuousDeploymentPolicyResponse)

instance
  Prelude.NFData
    UpdateContinuousDeploymentPolicyResponse
  where
  rnf UpdateContinuousDeploymentPolicyResponse' {..} =
    Prelude.rnf continuousDeploymentPolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
