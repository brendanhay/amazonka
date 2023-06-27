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
-- Module      : Amazonka.CloudFront.UpdateOriginRequestPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an origin request policy configuration.
--
-- When you update an origin request policy configuration, all the fields
-- are updated with the values provided in the request. You cannot update
-- some fields independent of others. To update an origin request policy
-- configuration:
--
-- 1.  Use @GetOriginRequestPolicyConfig@ to get the current configuration.
--
-- 2.  Locally modify the fields in the origin request policy configuration
--     that you want to update.
--
-- 3.  Call @UpdateOriginRequestPolicy@ by providing the entire origin
--     request policy configuration, including the fields that you modified
--     and those that you didn\'t.
module Amazonka.CloudFront.UpdateOriginRequestPolicy
  ( -- * Creating a Request
    UpdateOriginRequestPolicy (..),
    newUpdateOriginRequestPolicy,

    -- * Request Lenses
    updateOriginRequestPolicy_ifMatch,
    updateOriginRequestPolicy_originRequestPolicyConfig,
    updateOriginRequestPolicy_id,

    -- * Destructuring the Response
    UpdateOriginRequestPolicyResponse (..),
    newUpdateOriginRequestPolicyResponse,

    -- * Response Lenses
    updateOriginRequestPolicyResponse_eTag,
    updateOriginRequestPolicyResponse_originRequestPolicy,
    updateOriginRequestPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOriginRequestPolicy' smart constructor.
data UpdateOriginRequestPolicy = UpdateOriginRequestPolicy'
  { -- | The version of the origin request policy that you are updating. The
    -- version is returned in the origin request policy\'s @ETag@ field in the
    -- response to @GetOriginRequestPolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | An origin request policy configuration.
    originRequestPolicyConfig :: OriginRequestPolicyConfig,
    -- | The unique identifier for the origin request policy that you are
    -- updating. The identifier is returned in a cache behavior\'s
    -- @OriginRequestPolicyId@ field in the response to
    -- @GetDistributionConfig@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOriginRequestPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateOriginRequestPolicy_ifMatch' - The version of the origin request policy that you are updating. The
-- version is returned in the origin request policy\'s @ETag@ field in the
-- response to @GetOriginRequestPolicyConfig@.
--
-- 'originRequestPolicyConfig', 'updateOriginRequestPolicy_originRequestPolicyConfig' - An origin request policy configuration.
--
-- 'id', 'updateOriginRequestPolicy_id' - The unique identifier for the origin request policy that you are
-- updating. The identifier is returned in a cache behavior\'s
-- @OriginRequestPolicyId@ field in the response to
-- @GetDistributionConfig@.
newUpdateOriginRequestPolicy ::
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateOriginRequestPolicy
newUpdateOriginRequestPolicy
  pOriginRequestPolicyConfig_
  pId_ =
    UpdateOriginRequestPolicy'
      { ifMatch =
          Prelude.Nothing,
        originRequestPolicyConfig =
          pOriginRequestPolicyConfig_,
        id = pId_
      }

-- | The version of the origin request policy that you are updating. The
-- version is returned in the origin request policy\'s @ETag@ field in the
-- response to @GetOriginRequestPolicyConfig@.
updateOriginRequestPolicy_ifMatch :: Lens.Lens' UpdateOriginRequestPolicy (Prelude.Maybe Prelude.Text)
updateOriginRequestPolicy_ifMatch = Lens.lens (\UpdateOriginRequestPolicy' {ifMatch} -> ifMatch) (\s@UpdateOriginRequestPolicy' {} a -> s {ifMatch = a} :: UpdateOriginRequestPolicy)

-- | An origin request policy configuration.
updateOriginRequestPolicy_originRequestPolicyConfig :: Lens.Lens' UpdateOriginRequestPolicy OriginRequestPolicyConfig
updateOriginRequestPolicy_originRequestPolicyConfig = Lens.lens (\UpdateOriginRequestPolicy' {originRequestPolicyConfig} -> originRequestPolicyConfig) (\s@UpdateOriginRequestPolicy' {} a -> s {originRequestPolicyConfig = a} :: UpdateOriginRequestPolicy)

-- | The unique identifier for the origin request policy that you are
-- updating. The identifier is returned in a cache behavior\'s
-- @OriginRequestPolicyId@ field in the response to
-- @GetDistributionConfig@.
updateOriginRequestPolicy_id :: Lens.Lens' UpdateOriginRequestPolicy Prelude.Text
updateOriginRequestPolicy_id = Lens.lens (\UpdateOriginRequestPolicy' {id} -> id) (\s@UpdateOriginRequestPolicy' {} a -> s {id = a} :: UpdateOriginRequestPolicy)

instance Core.AWSRequest UpdateOriginRequestPolicy where
  type
    AWSResponse UpdateOriginRequestPolicy =
      UpdateOriginRequestPolicyResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateOriginRequestPolicyResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOriginRequestPolicy where
  hashWithSalt _salt UpdateOriginRequestPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` originRequestPolicyConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateOriginRequestPolicy where
  rnf UpdateOriginRequestPolicy' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf originRequestPolicyConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateOriginRequestPolicy where
  toElement UpdateOriginRequestPolicy' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginRequestPolicyConfig"
      originRequestPolicyConfig

instance Data.ToHeaders UpdateOriginRequestPolicy where
  toHeaders UpdateOriginRequestPolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateOriginRequestPolicy where
  toPath UpdateOriginRequestPolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/origin-request-policy/", Data.toBS id]

instance Data.ToQuery UpdateOriginRequestPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOriginRequestPolicyResponse' smart constructor.
data UpdateOriginRequestPolicyResponse = UpdateOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | An origin request policy.
    originRequestPolicy :: Prelude.Maybe OriginRequestPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOriginRequestPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateOriginRequestPolicyResponse_eTag' - The current version of the origin request policy.
--
-- 'originRequestPolicy', 'updateOriginRequestPolicyResponse_originRequestPolicy' - An origin request policy.
--
-- 'httpStatus', 'updateOriginRequestPolicyResponse_httpStatus' - The response's http status code.
newUpdateOriginRequestPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOriginRequestPolicyResponse
newUpdateOriginRequestPolicyResponse pHttpStatus_ =
  UpdateOriginRequestPolicyResponse'
    { eTag =
        Prelude.Nothing,
      originRequestPolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the origin request policy.
updateOriginRequestPolicyResponse_eTag :: Lens.Lens' UpdateOriginRequestPolicyResponse (Prelude.Maybe Prelude.Text)
updateOriginRequestPolicyResponse_eTag = Lens.lens (\UpdateOriginRequestPolicyResponse' {eTag} -> eTag) (\s@UpdateOriginRequestPolicyResponse' {} a -> s {eTag = a} :: UpdateOriginRequestPolicyResponse)

-- | An origin request policy.
updateOriginRequestPolicyResponse_originRequestPolicy :: Lens.Lens' UpdateOriginRequestPolicyResponse (Prelude.Maybe OriginRequestPolicy)
updateOriginRequestPolicyResponse_originRequestPolicy = Lens.lens (\UpdateOriginRequestPolicyResponse' {originRequestPolicy} -> originRequestPolicy) (\s@UpdateOriginRequestPolicyResponse' {} a -> s {originRequestPolicy = a} :: UpdateOriginRequestPolicyResponse)

-- | The response's http status code.
updateOriginRequestPolicyResponse_httpStatus :: Lens.Lens' UpdateOriginRequestPolicyResponse Prelude.Int
updateOriginRequestPolicyResponse_httpStatus = Lens.lens (\UpdateOriginRequestPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateOriginRequestPolicyResponse' {} a -> s {httpStatus = a} :: UpdateOriginRequestPolicyResponse)

instance
  Prelude.NFData
    UpdateOriginRequestPolicyResponse
  where
  rnf UpdateOriginRequestPolicyResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf originRequestPolicy
      `Prelude.seq` Prelude.rnf httpStatus
