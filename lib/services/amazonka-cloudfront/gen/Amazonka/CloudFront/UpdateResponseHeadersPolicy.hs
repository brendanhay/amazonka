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
-- Module      : Amazonka.CloudFront.UpdateResponseHeadersPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a response headers policy.
--
-- When you update a response headers policy, the entire policy is
-- replaced. You cannot update some policy fields independent of others. To
-- update a response headers policy configuration:
--
-- 1.  Use @GetResponseHeadersPolicyConfig@ to get the current policy’s
--     configuration.
--
-- 2.  Modify the fields in the response headers policy configuration that
--     you want to update.
--
-- 3.  Call @UpdateResponseHeadersPolicy@, providing the entire response
--     headers policy configuration, including the fields that you modified
--     and those that you didn’t.
module Amazonka.CloudFront.UpdateResponseHeadersPolicy
  ( -- * Creating a Request
    UpdateResponseHeadersPolicy (..),
    newUpdateResponseHeadersPolicy,

    -- * Request Lenses
    updateResponseHeadersPolicy_ifMatch,
    updateResponseHeadersPolicy_responseHeadersPolicyConfig,
    updateResponseHeadersPolicy_id,

    -- * Destructuring the Response
    UpdateResponseHeadersPolicyResponse (..),
    newUpdateResponseHeadersPolicyResponse,

    -- * Response Lenses
    updateResponseHeadersPolicyResponse_eTag,
    updateResponseHeadersPolicyResponse_responseHeadersPolicy,
    updateResponseHeadersPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResponseHeadersPolicy' smart constructor.
data UpdateResponseHeadersPolicy = UpdateResponseHeadersPolicy'
  { -- | The version of the response headers policy that you are updating.
    --
    -- The version is returned in the cache policy’s @ETag@ field in the
    -- response to @GetResponseHeadersPolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | A response headers policy configuration.
    responseHeadersPolicyConfig :: ResponseHeadersPolicyConfig,
    -- | The identifier for the response headers policy that you are updating.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponseHeadersPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateResponseHeadersPolicy_ifMatch' - The version of the response headers policy that you are updating.
--
-- The version is returned in the cache policy’s @ETag@ field in the
-- response to @GetResponseHeadersPolicyConfig@.
--
-- 'responseHeadersPolicyConfig', 'updateResponseHeadersPolicy_responseHeadersPolicyConfig' - A response headers policy configuration.
--
-- 'id', 'updateResponseHeadersPolicy_id' - The identifier for the response headers policy that you are updating.
newUpdateResponseHeadersPolicy ::
  -- | 'responseHeadersPolicyConfig'
  ResponseHeadersPolicyConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateResponseHeadersPolicy
newUpdateResponseHeadersPolicy
  pResponseHeadersPolicyConfig_
  pId_ =
    UpdateResponseHeadersPolicy'
      { ifMatch =
          Prelude.Nothing,
        responseHeadersPolicyConfig =
          pResponseHeadersPolicyConfig_,
        id = pId_
      }

-- | The version of the response headers policy that you are updating.
--
-- The version is returned in the cache policy’s @ETag@ field in the
-- response to @GetResponseHeadersPolicyConfig@.
updateResponseHeadersPolicy_ifMatch :: Lens.Lens' UpdateResponseHeadersPolicy (Prelude.Maybe Prelude.Text)
updateResponseHeadersPolicy_ifMatch = Lens.lens (\UpdateResponseHeadersPolicy' {ifMatch} -> ifMatch) (\s@UpdateResponseHeadersPolicy' {} a -> s {ifMatch = a} :: UpdateResponseHeadersPolicy)

-- | A response headers policy configuration.
updateResponseHeadersPolicy_responseHeadersPolicyConfig :: Lens.Lens' UpdateResponseHeadersPolicy ResponseHeadersPolicyConfig
updateResponseHeadersPolicy_responseHeadersPolicyConfig = Lens.lens (\UpdateResponseHeadersPolicy' {responseHeadersPolicyConfig} -> responseHeadersPolicyConfig) (\s@UpdateResponseHeadersPolicy' {} a -> s {responseHeadersPolicyConfig = a} :: UpdateResponseHeadersPolicy)

-- | The identifier for the response headers policy that you are updating.
updateResponseHeadersPolicy_id :: Lens.Lens' UpdateResponseHeadersPolicy Prelude.Text
updateResponseHeadersPolicy_id = Lens.lens (\UpdateResponseHeadersPolicy' {id} -> id) (\s@UpdateResponseHeadersPolicy' {} a -> s {id = a} :: UpdateResponseHeadersPolicy)

instance Core.AWSRequest UpdateResponseHeadersPolicy where
  type
    AWSResponse UpdateResponseHeadersPolicy =
      UpdateResponseHeadersPolicyResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateResponseHeadersPolicyResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResponseHeadersPolicy where
  hashWithSalt _salt UpdateResponseHeadersPolicy' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` responseHeadersPolicyConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateResponseHeadersPolicy where
  rnf UpdateResponseHeadersPolicy' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf responseHeadersPolicyConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateResponseHeadersPolicy where
  toElement UpdateResponseHeadersPolicy' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ResponseHeadersPolicyConfig"
      responseHeadersPolicyConfig

instance Data.ToHeaders UpdateResponseHeadersPolicy where
  toHeaders UpdateResponseHeadersPolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateResponseHeadersPolicy where
  toPath UpdateResponseHeadersPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/response-headers-policy/",
        Data.toBS id
      ]

instance Data.ToQuery UpdateResponseHeadersPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResponseHeadersPolicyResponse' smart constructor.
data UpdateResponseHeadersPolicyResponse = UpdateResponseHeadersPolicyResponse'
  { -- | The current version of the response headers policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | A response headers policy.
    responseHeadersPolicy :: Prelude.Maybe ResponseHeadersPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponseHeadersPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateResponseHeadersPolicyResponse_eTag' - The current version of the response headers policy.
--
-- 'responseHeadersPolicy', 'updateResponseHeadersPolicyResponse_responseHeadersPolicy' - A response headers policy.
--
-- 'httpStatus', 'updateResponseHeadersPolicyResponse_httpStatus' - The response's http status code.
newUpdateResponseHeadersPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResponseHeadersPolicyResponse
newUpdateResponseHeadersPolicyResponse pHttpStatus_ =
  UpdateResponseHeadersPolicyResponse'
    { eTag =
        Prelude.Nothing,
      responseHeadersPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the response headers policy.
updateResponseHeadersPolicyResponse_eTag :: Lens.Lens' UpdateResponseHeadersPolicyResponse (Prelude.Maybe Prelude.Text)
updateResponseHeadersPolicyResponse_eTag = Lens.lens (\UpdateResponseHeadersPolicyResponse' {eTag} -> eTag) (\s@UpdateResponseHeadersPolicyResponse' {} a -> s {eTag = a} :: UpdateResponseHeadersPolicyResponse)

-- | A response headers policy.
updateResponseHeadersPolicyResponse_responseHeadersPolicy :: Lens.Lens' UpdateResponseHeadersPolicyResponse (Prelude.Maybe ResponseHeadersPolicy)
updateResponseHeadersPolicyResponse_responseHeadersPolicy = Lens.lens (\UpdateResponseHeadersPolicyResponse' {responseHeadersPolicy} -> responseHeadersPolicy) (\s@UpdateResponseHeadersPolicyResponse' {} a -> s {responseHeadersPolicy = a} :: UpdateResponseHeadersPolicyResponse)

-- | The response's http status code.
updateResponseHeadersPolicyResponse_httpStatus :: Lens.Lens' UpdateResponseHeadersPolicyResponse Prelude.Int
updateResponseHeadersPolicyResponse_httpStatus = Lens.lens (\UpdateResponseHeadersPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateResponseHeadersPolicyResponse' {} a -> s {httpStatus = a} :: UpdateResponseHeadersPolicyResponse)

instance
  Prelude.NFData
    UpdateResponseHeadersPolicyResponse
  where
  rnf UpdateResponseHeadersPolicyResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf responseHeadersPolicy
      `Prelude.seq` Prelude.rnf httpStatus
