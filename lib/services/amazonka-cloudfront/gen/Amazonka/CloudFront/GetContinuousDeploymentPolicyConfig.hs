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
-- Module      : Amazonka.CloudFront.GetContinuousDeploymentPolicyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets configuration information about a continuous deployment policy.
module Amazonka.CloudFront.GetContinuousDeploymentPolicyConfig
  ( -- * Creating a Request
    GetContinuousDeploymentPolicyConfig (..),
    newGetContinuousDeploymentPolicyConfig,

    -- * Request Lenses
    getContinuousDeploymentPolicyConfig_id,

    -- * Destructuring the Response
    GetContinuousDeploymentPolicyConfigResponse (..),
    newGetContinuousDeploymentPolicyConfigResponse,

    -- * Response Lenses
    getContinuousDeploymentPolicyConfigResponse_continuousDeploymentPolicyConfig,
    getContinuousDeploymentPolicyConfigResponse_eTag,
    getContinuousDeploymentPolicyConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContinuousDeploymentPolicyConfig' smart constructor.
data GetContinuousDeploymentPolicyConfig = GetContinuousDeploymentPolicyConfig'
  { -- | The identifier of the continuous deployment policy whose configuration
    -- you are getting.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContinuousDeploymentPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getContinuousDeploymentPolicyConfig_id' - The identifier of the continuous deployment policy whose configuration
-- you are getting.
newGetContinuousDeploymentPolicyConfig ::
  -- | 'id'
  Prelude.Text ->
  GetContinuousDeploymentPolicyConfig
newGetContinuousDeploymentPolicyConfig pId_ =
  GetContinuousDeploymentPolicyConfig' {id = pId_}

-- | The identifier of the continuous deployment policy whose configuration
-- you are getting.
getContinuousDeploymentPolicyConfig_id :: Lens.Lens' GetContinuousDeploymentPolicyConfig Prelude.Text
getContinuousDeploymentPolicyConfig_id = Lens.lens (\GetContinuousDeploymentPolicyConfig' {id} -> id) (\s@GetContinuousDeploymentPolicyConfig' {} a -> s {id = a} :: GetContinuousDeploymentPolicyConfig)

instance
  Core.AWSRequest
    GetContinuousDeploymentPolicyConfig
  where
  type
    AWSResponse GetContinuousDeploymentPolicyConfig =
      GetContinuousDeploymentPolicyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetContinuousDeploymentPolicyConfigResponse'
            Prelude.<$> (Core.parseXML x) Prelude.<*> (h Core..#? "ETag")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetContinuousDeploymentPolicyConfig
  where
  hashWithSalt
    _salt
    GetContinuousDeploymentPolicyConfig' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetContinuousDeploymentPolicyConfig
  where
  rnf GetContinuousDeploymentPolicyConfig' {..} =
    Prelude.rnf id

instance
  Core.ToHeaders
    GetContinuousDeploymentPolicyConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetContinuousDeploymentPolicyConfig
  where
  toPath GetContinuousDeploymentPolicyConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/continuous-deployment-policy/",
        Core.toBS id,
        "/config"
      ]

instance
  Core.ToQuery
    GetContinuousDeploymentPolicyConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContinuousDeploymentPolicyConfigResponse' smart constructor.
data GetContinuousDeploymentPolicyConfigResponse = GetContinuousDeploymentPolicyConfigResponse'
  { continuousDeploymentPolicyConfig :: Prelude.Maybe ContinuousDeploymentPolicyConfig,
    -- | The version identifier for the current version of the continuous
    -- deployment policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContinuousDeploymentPolicyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicyConfig', 'getContinuousDeploymentPolicyConfigResponse_continuousDeploymentPolicyConfig' - Undocumented member.
--
-- 'eTag', 'getContinuousDeploymentPolicyConfigResponse_eTag' - The version identifier for the current version of the continuous
-- deployment policy.
--
-- 'httpStatus', 'getContinuousDeploymentPolicyConfigResponse_httpStatus' - The response's http status code.
newGetContinuousDeploymentPolicyConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContinuousDeploymentPolicyConfigResponse
newGetContinuousDeploymentPolicyConfigResponse
  pHttpStatus_ =
    GetContinuousDeploymentPolicyConfigResponse'
      { continuousDeploymentPolicyConfig =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
getContinuousDeploymentPolicyConfigResponse_continuousDeploymentPolicyConfig :: Lens.Lens' GetContinuousDeploymentPolicyConfigResponse (Prelude.Maybe ContinuousDeploymentPolicyConfig)
getContinuousDeploymentPolicyConfigResponse_continuousDeploymentPolicyConfig = Lens.lens (\GetContinuousDeploymentPolicyConfigResponse' {continuousDeploymentPolicyConfig} -> continuousDeploymentPolicyConfig) (\s@GetContinuousDeploymentPolicyConfigResponse' {} a -> s {continuousDeploymentPolicyConfig = a} :: GetContinuousDeploymentPolicyConfigResponse)

-- | The version identifier for the current version of the continuous
-- deployment policy.
getContinuousDeploymentPolicyConfigResponse_eTag :: Lens.Lens' GetContinuousDeploymentPolicyConfigResponse (Prelude.Maybe Prelude.Text)
getContinuousDeploymentPolicyConfigResponse_eTag = Lens.lens (\GetContinuousDeploymentPolicyConfigResponse' {eTag} -> eTag) (\s@GetContinuousDeploymentPolicyConfigResponse' {} a -> s {eTag = a} :: GetContinuousDeploymentPolicyConfigResponse)

-- | The response's http status code.
getContinuousDeploymentPolicyConfigResponse_httpStatus :: Lens.Lens' GetContinuousDeploymentPolicyConfigResponse Prelude.Int
getContinuousDeploymentPolicyConfigResponse_httpStatus = Lens.lens (\GetContinuousDeploymentPolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetContinuousDeploymentPolicyConfigResponse' {} a -> s {httpStatus = a} :: GetContinuousDeploymentPolicyConfigResponse)

instance
  Prelude.NFData
    GetContinuousDeploymentPolicyConfigResponse
  where
  rnf GetContinuousDeploymentPolicyConfigResponse' {..} =
    Prelude.rnf continuousDeploymentPolicyConfig
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
