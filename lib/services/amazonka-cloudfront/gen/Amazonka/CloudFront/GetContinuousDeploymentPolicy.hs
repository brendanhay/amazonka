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
-- Module      : Amazonka.CloudFront.GetContinuousDeploymentPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a continuous deployment policy, including metadata (the policy\'s
-- identifier and the date and time when the policy was last modified).
module Amazonka.CloudFront.GetContinuousDeploymentPolicy
  ( -- * Creating a Request
    GetContinuousDeploymentPolicy (..),
    newGetContinuousDeploymentPolicy,

    -- * Request Lenses
    getContinuousDeploymentPolicy_id,

    -- * Destructuring the Response
    GetContinuousDeploymentPolicyResponse (..),
    newGetContinuousDeploymentPolicyResponse,

    -- * Response Lenses
    getContinuousDeploymentPolicyResponse_continuousDeploymentPolicy,
    getContinuousDeploymentPolicyResponse_eTag,
    getContinuousDeploymentPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContinuousDeploymentPolicy' smart constructor.
data GetContinuousDeploymentPolicy = GetContinuousDeploymentPolicy'
  { -- | The identifier of the continuous deployment policy that you are getting.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContinuousDeploymentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getContinuousDeploymentPolicy_id' - The identifier of the continuous deployment policy that you are getting.
newGetContinuousDeploymentPolicy ::
  -- | 'id'
  Prelude.Text ->
  GetContinuousDeploymentPolicy
newGetContinuousDeploymentPolicy pId_ =
  GetContinuousDeploymentPolicy' {id = pId_}

-- | The identifier of the continuous deployment policy that you are getting.
getContinuousDeploymentPolicy_id :: Lens.Lens' GetContinuousDeploymentPolicy Prelude.Text
getContinuousDeploymentPolicy_id = Lens.lens (\GetContinuousDeploymentPolicy' {id} -> id) (\s@GetContinuousDeploymentPolicy' {} a -> s {id = a} :: GetContinuousDeploymentPolicy)

instance
  Core.AWSRequest
    GetContinuousDeploymentPolicy
  where
  type
    AWSResponse GetContinuousDeploymentPolicy =
      GetContinuousDeploymentPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetContinuousDeploymentPolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetContinuousDeploymentPolicy
  where
  hashWithSalt _salt GetContinuousDeploymentPolicy' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetContinuousDeploymentPolicy where
  rnf GetContinuousDeploymentPolicy' {..} =
    Prelude.rnf id

instance Data.ToHeaders GetContinuousDeploymentPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetContinuousDeploymentPolicy where
  toPath GetContinuousDeploymentPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/continuous-deployment-policy/",
        Data.toBS id
      ]

instance Data.ToQuery GetContinuousDeploymentPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContinuousDeploymentPolicyResponse' smart constructor.
data GetContinuousDeploymentPolicyResponse = GetContinuousDeploymentPolicyResponse'
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
-- Create a value of 'GetContinuousDeploymentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicy', 'getContinuousDeploymentPolicyResponse_continuousDeploymentPolicy' - A continuous deployment policy.
--
-- 'eTag', 'getContinuousDeploymentPolicyResponse_eTag' - The version identifier for the current version of the continuous
-- deployment policy.
--
-- 'httpStatus', 'getContinuousDeploymentPolicyResponse_httpStatus' - The response's http status code.
newGetContinuousDeploymentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContinuousDeploymentPolicyResponse
newGetContinuousDeploymentPolicyResponse pHttpStatus_ =
  GetContinuousDeploymentPolicyResponse'
    { continuousDeploymentPolicy =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuous deployment policy.
getContinuousDeploymentPolicyResponse_continuousDeploymentPolicy :: Lens.Lens' GetContinuousDeploymentPolicyResponse (Prelude.Maybe ContinuousDeploymentPolicy)
getContinuousDeploymentPolicyResponse_continuousDeploymentPolicy = Lens.lens (\GetContinuousDeploymentPolicyResponse' {continuousDeploymentPolicy} -> continuousDeploymentPolicy) (\s@GetContinuousDeploymentPolicyResponse' {} a -> s {continuousDeploymentPolicy = a} :: GetContinuousDeploymentPolicyResponse)

-- | The version identifier for the current version of the continuous
-- deployment policy.
getContinuousDeploymentPolicyResponse_eTag :: Lens.Lens' GetContinuousDeploymentPolicyResponse (Prelude.Maybe Prelude.Text)
getContinuousDeploymentPolicyResponse_eTag = Lens.lens (\GetContinuousDeploymentPolicyResponse' {eTag} -> eTag) (\s@GetContinuousDeploymentPolicyResponse' {} a -> s {eTag = a} :: GetContinuousDeploymentPolicyResponse)

-- | The response's http status code.
getContinuousDeploymentPolicyResponse_httpStatus :: Lens.Lens' GetContinuousDeploymentPolicyResponse Prelude.Int
getContinuousDeploymentPolicyResponse_httpStatus = Lens.lens (\GetContinuousDeploymentPolicyResponse' {httpStatus} -> httpStatus) (\s@GetContinuousDeploymentPolicyResponse' {} a -> s {httpStatus = a} :: GetContinuousDeploymentPolicyResponse)

instance
  Prelude.NFData
    GetContinuousDeploymentPolicyResponse
  where
  rnf GetContinuousDeploymentPolicyResponse' {..} =
    Prelude.rnf continuousDeploymentPolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
