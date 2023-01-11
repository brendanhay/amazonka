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
-- Module      : Amazonka.Shield.DisableApplicationLayerAutomaticResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable the Shield Advanced automatic application layer DDoS mitigation
-- feature for the protected resource. This stops Shield Advanced from
-- creating, verifying, and applying WAF rules for attacks that it detects
-- for the resource.
module Amazonka.Shield.DisableApplicationLayerAutomaticResponse
  ( -- * Creating a Request
    DisableApplicationLayerAutomaticResponse (..),
    newDisableApplicationLayerAutomaticResponse,

    -- * Request Lenses
    disableApplicationLayerAutomaticResponse_resourceArn,

    -- * Destructuring the Response
    DisableApplicationLayerAutomaticResponseResponse (..),
    newDisableApplicationLayerAutomaticResponseResponse,

    -- * Response Lenses
    disableApplicationLayerAutomaticResponseResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDisableApplicationLayerAutomaticResponse' smart constructor.
data DisableApplicationLayerAutomaticResponse = DisableApplicationLayerAutomaticResponse'
  { -- | The ARN (Amazon Resource Name) of the protected resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableApplicationLayerAutomaticResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'disableApplicationLayerAutomaticResponse_resourceArn' - The ARN (Amazon Resource Name) of the protected resource.
newDisableApplicationLayerAutomaticResponse ::
  -- | 'resourceArn'
  Prelude.Text ->
  DisableApplicationLayerAutomaticResponse
newDisableApplicationLayerAutomaticResponse
  pResourceArn_ =
    DisableApplicationLayerAutomaticResponse'
      { resourceArn =
          pResourceArn_
      }

-- | The ARN (Amazon Resource Name) of the protected resource.
disableApplicationLayerAutomaticResponse_resourceArn :: Lens.Lens' DisableApplicationLayerAutomaticResponse Prelude.Text
disableApplicationLayerAutomaticResponse_resourceArn = Lens.lens (\DisableApplicationLayerAutomaticResponse' {resourceArn} -> resourceArn) (\s@DisableApplicationLayerAutomaticResponse' {} a -> s {resourceArn = a} :: DisableApplicationLayerAutomaticResponse)

instance
  Core.AWSRequest
    DisableApplicationLayerAutomaticResponse
  where
  type
    AWSResponse
      DisableApplicationLayerAutomaticResponse =
      DisableApplicationLayerAutomaticResponseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableApplicationLayerAutomaticResponseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableApplicationLayerAutomaticResponse
  where
  hashWithSalt
    _salt
    DisableApplicationLayerAutomaticResponse' {..} =
      _salt `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    DisableApplicationLayerAutomaticResponse
  where
  rnf DisableApplicationLayerAutomaticResponse' {..} =
    Prelude.rnf resourceArn

instance
  Data.ToHeaders
    DisableApplicationLayerAutomaticResponse
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DisableApplicationLayerAutomaticResponse" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisableApplicationLayerAutomaticResponse
  where
  toJSON DisableApplicationLayerAutomaticResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance
  Data.ToPath
    DisableApplicationLayerAutomaticResponse
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisableApplicationLayerAutomaticResponse
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableApplicationLayerAutomaticResponseResponse' smart constructor.
data DisableApplicationLayerAutomaticResponseResponse = DisableApplicationLayerAutomaticResponseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableApplicationLayerAutomaticResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableApplicationLayerAutomaticResponseResponse_httpStatus' - The response's http status code.
newDisableApplicationLayerAutomaticResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableApplicationLayerAutomaticResponseResponse
newDisableApplicationLayerAutomaticResponseResponse
  pHttpStatus_ =
    DisableApplicationLayerAutomaticResponseResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disableApplicationLayerAutomaticResponseResponse_httpStatus :: Lens.Lens' DisableApplicationLayerAutomaticResponseResponse Prelude.Int
disableApplicationLayerAutomaticResponseResponse_httpStatus = Lens.lens (\DisableApplicationLayerAutomaticResponseResponse' {httpStatus} -> httpStatus) (\s@DisableApplicationLayerAutomaticResponseResponse' {} a -> s {httpStatus = a} :: DisableApplicationLayerAutomaticResponseResponse)

instance
  Prelude.NFData
    DisableApplicationLayerAutomaticResponseResponse
  where
  rnf
    DisableApplicationLayerAutomaticResponseResponse' {..} =
      Prelude.rnf httpStatus
