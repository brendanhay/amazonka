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
-- Module      : Amazonka.SESV2.PutDedicatedIpPoolScalingAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to convert a dedicated IP pool to a different scaling mode.
--
-- @MANAGED@ pools cannot be converted to @STANDARD@ scaling mode.
module Amazonka.SESV2.PutDedicatedIpPoolScalingAttributes
  ( -- * Creating a Request
    PutDedicatedIpPoolScalingAttributes (..),
    newPutDedicatedIpPoolScalingAttributes,

    -- * Request Lenses
    putDedicatedIpPoolScalingAttributes_poolName,
    putDedicatedIpPoolScalingAttributes_scalingMode,

    -- * Destructuring the Response
    PutDedicatedIpPoolScalingAttributesResponse (..),
    newPutDedicatedIpPoolScalingAttributesResponse,

    -- * Response Lenses
    putDedicatedIpPoolScalingAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to convert a dedicated IP pool to a different scaling mode.
--
-- /See:/ 'newPutDedicatedIpPoolScalingAttributes' smart constructor.
data PutDedicatedIpPoolScalingAttributes = PutDedicatedIpPoolScalingAttributes'
  { -- | The name of the dedicated IP pool.
    poolName :: Prelude.Text,
    -- | The scaling mode to apply to the dedicated IP pool.
    --
    -- Changing the scaling mode from @MANAGED@ to @STANDARD@ is not supported.
    scalingMode :: ScalingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpPoolScalingAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolName', 'putDedicatedIpPoolScalingAttributes_poolName' - The name of the dedicated IP pool.
--
-- 'scalingMode', 'putDedicatedIpPoolScalingAttributes_scalingMode' - The scaling mode to apply to the dedicated IP pool.
--
-- Changing the scaling mode from @MANAGED@ to @STANDARD@ is not supported.
newPutDedicatedIpPoolScalingAttributes ::
  -- | 'poolName'
  Prelude.Text ->
  -- | 'scalingMode'
  ScalingMode ->
  PutDedicatedIpPoolScalingAttributes
newPutDedicatedIpPoolScalingAttributes
  pPoolName_
  pScalingMode_ =
    PutDedicatedIpPoolScalingAttributes'
      { poolName =
          pPoolName_,
        scalingMode = pScalingMode_
      }

-- | The name of the dedicated IP pool.
putDedicatedIpPoolScalingAttributes_poolName :: Lens.Lens' PutDedicatedIpPoolScalingAttributes Prelude.Text
putDedicatedIpPoolScalingAttributes_poolName = Lens.lens (\PutDedicatedIpPoolScalingAttributes' {poolName} -> poolName) (\s@PutDedicatedIpPoolScalingAttributes' {} a -> s {poolName = a} :: PutDedicatedIpPoolScalingAttributes)

-- | The scaling mode to apply to the dedicated IP pool.
--
-- Changing the scaling mode from @MANAGED@ to @STANDARD@ is not supported.
putDedicatedIpPoolScalingAttributes_scalingMode :: Lens.Lens' PutDedicatedIpPoolScalingAttributes ScalingMode
putDedicatedIpPoolScalingAttributes_scalingMode = Lens.lens (\PutDedicatedIpPoolScalingAttributes' {scalingMode} -> scalingMode) (\s@PutDedicatedIpPoolScalingAttributes' {} a -> s {scalingMode = a} :: PutDedicatedIpPoolScalingAttributes)

instance
  Core.AWSRequest
    PutDedicatedIpPoolScalingAttributes
  where
  type
    AWSResponse PutDedicatedIpPoolScalingAttributes =
      PutDedicatedIpPoolScalingAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutDedicatedIpPoolScalingAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutDedicatedIpPoolScalingAttributes
  where
  hashWithSalt
    _salt
    PutDedicatedIpPoolScalingAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` poolName
        `Prelude.hashWithSalt` scalingMode

instance
  Prelude.NFData
    PutDedicatedIpPoolScalingAttributes
  where
  rnf PutDedicatedIpPoolScalingAttributes' {..} =
    Prelude.rnf poolName
      `Prelude.seq` Prelude.rnf scalingMode

instance
  Data.ToHeaders
    PutDedicatedIpPoolScalingAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PutDedicatedIpPoolScalingAttributes
  where
  toJSON PutDedicatedIpPoolScalingAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ScalingMode" Data..= scalingMode)]
      )

instance
  Data.ToPath
    PutDedicatedIpPoolScalingAttributes
  where
  toPath PutDedicatedIpPoolScalingAttributes' {..} =
    Prelude.mconcat
      [ "/v2/email/dedicated-ip-pools/",
        Data.toBS poolName,
        "/scaling"
      ]

instance
  Data.ToQuery
    PutDedicatedIpPoolScalingAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutDedicatedIpPoolScalingAttributesResponse' smart constructor.
data PutDedicatedIpPoolScalingAttributesResponse = PutDedicatedIpPoolScalingAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDedicatedIpPoolScalingAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putDedicatedIpPoolScalingAttributesResponse_httpStatus' - The response's http status code.
newPutDedicatedIpPoolScalingAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDedicatedIpPoolScalingAttributesResponse
newPutDedicatedIpPoolScalingAttributesResponse
  pHttpStatus_ =
    PutDedicatedIpPoolScalingAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putDedicatedIpPoolScalingAttributesResponse_httpStatus :: Lens.Lens' PutDedicatedIpPoolScalingAttributesResponse Prelude.Int
putDedicatedIpPoolScalingAttributesResponse_httpStatus = Lens.lens (\PutDedicatedIpPoolScalingAttributesResponse' {httpStatus} -> httpStatus) (\s@PutDedicatedIpPoolScalingAttributesResponse' {} a -> s {httpStatus = a} :: PutDedicatedIpPoolScalingAttributesResponse)

instance
  Prelude.NFData
    PutDedicatedIpPoolScalingAttributesResponse
  where
  rnf PutDedicatedIpPoolScalingAttributesResponse' {..} =
    Prelude.rnf httpStatus
