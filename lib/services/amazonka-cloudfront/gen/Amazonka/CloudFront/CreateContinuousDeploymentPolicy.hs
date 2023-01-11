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
-- Module      : Amazonka.CloudFront.CreateContinuousDeploymentPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a continuous deployment policy that distributes traffic for a
-- custom domain name to two different CloudFront distributions.
--
-- To use a continuous deployment policy, first use @CopyDistribution@ to
-- create a staging distribution, then use @UpdateDistribution@ to modify
-- the staging distribution\'s configuration.
--
-- After you create and update a staging distribution, you can use a
-- continuous deployment policy to incrementally move traffic to the
-- staging distribution. This workflow enables you to test changes to a
-- distribution\'s configuration before moving all of your domain\'s
-- production traffic to the new configuration.
module Amazonka.CloudFront.CreateContinuousDeploymentPolicy
  ( -- * Creating a Request
    CreateContinuousDeploymentPolicy (..),
    newCreateContinuousDeploymentPolicy,

    -- * Request Lenses
    createContinuousDeploymentPolicy_continuousDeploymentPolicyConfig,

    -- * Destructuring the Response
    CreateContinuousDeploymentPolicyResponse (..),
    newCreateContinuousDeploymentPolicyResponse,

    -- * Response Lenses
    createContinuousDeploymentPolicyResponse_continuousDeploymentPolicy,
    createContinuousDeploymentPolicyResponse_eTag,
    createContinuousDeploymentPolicyResponse_location,
    createContinuousDeploymentPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContinuousDeploymentPolicy' smart constructor.
data CreateContinuousDeploymentPolicy = CreateContinuousDeploymentPolicy'
  { -- | Contains the configuration for a continuous deployment policy.
    continuousDeploymentPolicyConfig :: ContinuousDeploymentPolicyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContinuousDeploymentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicyConfig', 'createContinuousDeploymentPolicy_continuousDeploymentPolicyConfig' - Contains the configuration for a continuous deployment policy.
newCreateContinuousDeploymentPolicy ::
  -- | 'continuousDeploymentPolicyConfig'
  ContinuousDeploymentPolicyConfig ->
  CreateContinuousDeploymentPolicy
newCreateContinuousDeploymentPolicy
  pContinuousDeploymentPolicyConfig_ =
    CreateContinuousDeploymentPolicy'
      { continuousDeploymentPolicyConfig =
          pContinuousDeploymentPolicyConfig_
      }

-- | Contains the configuration for a continuous deployment policy.
createContinuousDeploymentPolicy_continuousDeploymentPolicyConfig :: Lens.Lens' CreateContinuousDeploymentPolicy ContinuousDeploymentPolicyConfig
createContinuousDeploymentPolicy_continuousDeploymentPolicyConfig = Lens.lens (\CreateContinuousDeploymentPolicy' {continuousDeploymentPolicyConfig} -> continuousDeploymentPolicyConfig) (\s@CreateContinuousDeploymentPolicy' {} a -> s {continuousDeploymentPolicyConfig = a} :: CreateContinuousDeploymentPolicy)

instance
  Core.AWSRequest
    CreateContinuousDeploymentPolicy
  where
  type
    AWSResponse CreateContinuousDeploymentPolicy =
      CreateContinuousDeploymentPolicyResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateContinuousDeploymentPolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateContinuousDeploymentPolicy
  where
  hashWithSalt
    _salt
    CreateContinuousDeploymentPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` continuousDeploymentPolicyConfig

instance
  Prelude.NFData
    CreateContinuousDeploymentPolicy
  where
  rnf CreateContinuousDeploymentPolicy' {..} =
    Prelude.rnf continuousDeploymentPolicyConfig

instance
  Data.ToElement
    CreateContinuousDeploymentPolicy
  where
  toElement CreateContinuousDeploymentPolicy' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ContinuousDeploymentPolicyConfig"
      continuousDeploymentPolicyConfig

instance
  Data.ToHeaders
    CreateContinuousDeploymentPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateContinuousDeploymentPolicy where
  toPath =
    Prelude.const
      "/2020-05-31/continuous-deployment-policy"

instance
  Data.ToQuery
    CreateContinuousDeploymentPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContinuousDeploymentPolicyResponse' smart constructor.
data CreateContinuousDeploymentPolicyResponse = CreateContinuousDeploymentPolicyResponse'
  { -- | A continuous deployment policy.
    continuousDeploymentPolicy :: Prelude.Maybe ContinuousDeploymentPolicy,
    -- | The version identifier for the current version of the continuous
    -- deployment policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The location of the continuous deployment policy.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContinuousDeploymentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicy', 'createContinuousDeploymentPolicyResponse_continuousDeploymentPolicy' - A continuous deployment policy.
--
-- 'eTag', 'createContinuousDeploymentPolicyResponse_eTag' - The version identifier for the current version of the continuous
-- deployment policy.
--
-- 'location', 'createContinuousDeploymentPolicyResponse_location' - The location of the continuous deployment policy.
--
-- 'httpStatus', 'createContinuousDeploymentPolicyResponse_httpStatus' - The response's http status code.
newCreateContinuousDeploymentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContinuousDeploymentPolicyResponse
newCreateContinuousDeploymentPolicyResponse
  pHttpStatus_ =
    CreateContinuousDeploymentPolicyResponse'
      { continuousDeploymentPolicy =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        location = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A continuous deployment policy.
createContinuousDeploymentPolicyResponse_continuousDeploymentPolicy :: Lens.Lens' CreateContinuousDeploymentPolicyResponse (Prelude.Maybe ContinuousDeploymentPolicy)
createContinuousDeploymentPolicyResponse_continuousDeploymentPolicy = Lens.lens (\CreateContinuousDeploymentPolicyResponse' {continuousDeploymentPolicy} -> continuousDeploymentPolicy) (\s@CreateContinuousDeploymentPolicyResponse' {} a -> s {continuousDeploymentPolicy = a} :: CreateContinuousDeploymentPolicyResponse)

-- | The version identifier for the current version of the continuous
-- deployment policy.
createContinuousDeploymentPolicyResponse_eTag :: Lens.Lens' CreateContinuousDeploymentPolicyResponse (Prelude.Maybe Prelude.Text)
createContinuousDeploymentPolicyResponse_eTag = Lens.lens (\CreateContinuousDeploymentPolicyResponse' {eTag} -> eTag) (\s@CreateContinuousDeploymentPolicyResponse' {} a -> s {eTag = a} :: CreateContinuousDeploymentPolicyResponse)

-- | The location of the continuous deployment policy.
createContinuousDeploymentPolicyResponse_location :: Lens.Lens' CreateContinuousDeploymentPolicyResponse (Prelude.Maybe Prelude.Text)
createContinuousDeploymentPolicyResponse_location = Lens.lens (\CreateContinuousDeploymentPolicyResponse' {location} -> location) (\s@CreateContinuousDeploymentPolicyResponse' {} a -> s {location = a} :: CreateContinuousDeploymentPolicyResponse)

-- | The response's http status code.
createContinuousDeploymentPolicyResponse_httpStatus :: Lens.Lens' CreateContinuousDeploymentPolicyResponse Prelude.Int
createContinuousDeploymentPolicyResponse_httpStatus = Lens.lens (\CreateContinuousDeploymentPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateContinuousDeploymentPolicyResponse' {} a -> s {httpStatus = a} :: CreateContinuousDeploymentPolicyResponse)

instance
  Prelude.NFData
    CreateContinuousDeploymentPolicyResponse
  where
  rnf CreateContinuousDeploymentPolicyResponse' {..} =
    Prelude.rnf continuousDeploymentPolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
