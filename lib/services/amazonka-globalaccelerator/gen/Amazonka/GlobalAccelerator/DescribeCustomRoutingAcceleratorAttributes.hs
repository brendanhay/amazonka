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
-- Module      : Amazonka.GlobalAccelerator.DescribeCustomRoutingAcceleratorAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe the attributes of a custom routing accelerator.
module Amazonka.GlobalAccelerator.DescribeCustomRoutingAcceleratorAttributes
  ( -- * Creating a Request
    DescribeCustomRoutingAcceleratorAttributes (..),
    newDescribeCustomRoutingAcceleratorAttributes,

    -- * Request Lenses
    describeCustomRoutingAcceleratorAttributes_acceleratorArn,

    -- * Destructuring the Response
    DescribeCustomRoutingAcceleratorAttributesResponse (..),
    newDescribeCustomRoutingAcceleratorAttributesResponse,

    -- * Response Lenses
    describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    describeCustomRoutingAcceleratorAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomRoutingAcceleratorAttributes' smart constructor.
data DescribeCustomRoutingAcceleratorAttributes = DescribeCustomRoutingAcceleratorAttributes'
  { -- | The Amazon Resource Name (ARN) of the custom routing accelerator to
    -- describe the attributes for.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingAcceleratorAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'describeCustomRoutingAcceleratorAttributes_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator to
-- describe the attributes for.
newDescribeCustomRoutingAcceleratorAttributes ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  DescribeCustomRoutingAcceleratorAttributes
newDescribeCustomRoutingAcceleratorAttributes
  pAcceleratorArn_ =
    DescribeCustomRoutingAcceleratorAttributes'
      { acceleratorArn =
          pAcceleratorArn_
      }

-- | The Amazon Resource Name (ARN) of the custom routing accelerator to
-- describe the attributes for.
describeCustomRoutingAcceleratorAttributes_acceleratorArn :: Lens.Lens' DescribeCustomRoutingAcceleratorAttributes Prelude.Text
describeCustomRoutingAcceleratorAttributes_acceleratorArn = Lens.lens (\DescribeCustomRoutingAcceleratorAttributes' {acceleratorArn} -> acceleratorArn) (\s@DescribeCustomRoutingAcceleratorAttributes' {} a -> s {acceleratorArn = a} :: DescribeCustomRoutingAcceleratorAttributes)

instance
  Core.AWSRequest
    DescribeCustomRoutingAcceleratorAttributes
  where
  type
    AWSResponse
      DescribeCustomRoutingAcceleratorAttributes =
      DescribeCustomRoutingAcceleratorAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomRoutingAcceleratorAttributesResponse'
            Prelude.<$> (x Data..?> "AcceleratorAttributes")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomRoutingAcceleratorAttributes
  where
  hashWithSalt
    _salt
    DescribeCustomRoutingAcceleratorAttributes' {..} =
      _salt `Prelude.hashWithSalt` acceleratorArn

instance
  Prelude.NFData
    DescribeCustomRoutingAcceleratorAttributes
  where
  rnf DescribeCustomRoutingAcceleratorAttributes' {..} =
    Prelude.rnf acceleratorArn

instance
  Data.ToHeaders
    DescribeCustomRoutingAcceleratorAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DescribeCustomRoutingAcceleratorAttributes" ::
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
    DescribeCustomRoutingAcceleratorAttributes
  where
  toJSON
    DescribeCustomRoutingAcceleratorAttributes' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just
                ("AcceleratorArn" Data..= acceleratorArn)
            ]
        )

instance
  Data.ToPath
    DescribeCustomRoutingAcceleratorAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeCustomRoutingAcceleratorAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomRoutingAcceleratorAttributesResponse' smart constructor.
data DescribeCustomRoutingAcceleratorAttributesResponse = DescribeCustomRoutingAcceleratorAttributesResponse'
  { -- | The attributes of the custom routing accelerator.
    acceleratorAttributes :: Prelude.Maybe CustomRoutingAcceleratorAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingAcceleratorAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorAttributes', 'describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes' - The attributes of the custom routing accelerator.
--
-- 'httpStatus', 'describeCustomRoutingAcceleratorAttributesResponse_httpStatus' - The response's http status code.
newDescribeCustomRoutingAcceleratorAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomRoutingAcceleratorAttributesResponse
newDescribeCustomRoutingAcceleratorAttributesResponse
  pHttpStatus_ =
    DescribeCustomRoutingAcceleratorAttributesResponse'
      { acceleratorAttributes =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The attributes of the custom routing accelerator.
describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes :: Lens.Lens' DescribeCustomRoutingAcceleratorAttributesResponse (Prelude.Maybe CustomRoutingAcceleratorAttributes)
describeCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes = Lens.lens (\DescribeCustomRoutingAcceleratorAttributesResponse' {acceleratorAttributes} -> acceleratorAttributes) (\s@DescribeCustomRoutingAcceleratorAttributesResponse' {} a -> s {acceleratorAttributes = a} :: DescribeCustomRoutingAcceleratorAttributesResponse)

-- | The response's http status code.
describeCustomRoutingAcceleratorAttributesResponse_httpStatus :: Lens.Lens' DescribeCustomRoutingAcceleratorAttributesResponse Prelude.Int
describeCustomRoutingAcceleratorAttributesResponse_httpStatus = Lens.lens (\DescribeCustomRoutingAcceleratorAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomRoutingAcceleratorAttributesResponse' {} a -> s {httpStatus = a} :: DescribeCustomRoutingAcceleratorAttributesResponse)

instance
  Prelude.NFData
    DescribeCustomRoutingAcceleratorAttributesResponse
  where
  rnf
    DescribeCustomRoutingAcceleratorAttributesResponse' {..} =
      Prelude.rnf acceleratorAttributes
        `Prelude.seq` Prelude.rnf httpStatus
