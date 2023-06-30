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
-- Module      : Amazonka.EC2.DescribeFpgaImageAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified Amazon FPGA Image
-- (AFI).
module Amazonka.EC2.DescribeFpgaImageAttribute
  ( -- * Creating a Request
    DescribeFpgaImageAttribute (..),
    newDescribeFpgaImageAttribute,

    -- * Request Lenses
    describeFpgaImageAttribute_dryRun,
    describeFpgaImageAttribute_fpgaImageId,
    describeFpgaImageAttribute_attribute,

    -- * Destructuring the Response
    DescribeFpgaImageAttributeResponse (..),
    newDescribeFpgaImageAttributeResponse,

    -- * Response Lenses
    describeFpgaImageAttributeResponse_fpgaImageAttribute,
    describeFpgaImageAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFpgaImageAttribute' smart constructor.
data DescribeFpgaImageAttribute = DescribeFpgaImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Text,
    -- | The AFI attribute.
    attribute :: FpgaImageAttributeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeFpgaImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'fpgaImageId', 'describeFpgaImageAttribute_fpgaImageId' - The ID of the AFI.
--
-- 'attribute', 'describeFpgaImageAttribute_attribute' - The AFI attribute.
newDescribeFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Prelude.Text ->
  -- | 'attribute'
  FpgaImageAttributeName ->
  DescribeFpgaImageAttribute
newDescribeFpgaImageAttribute
  pFpgaImageId_
  pAttribute_ =
    DescribeFpgaImageAttribute'
      { dryRun =
          Prelude.Nothing,
        fpgaImageId = pFpgaImageId_,
        attribute = pAttribute_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFpgaImageAttribute_dryRun :: Lens.Lens' DescribeFpgaImageAttribute (Prelude.Maybe Prelude.Bool)
describeFpgaImageAttribute_dryRun = Lens.lens (\DescribeFpgaImageAttribute' {dryRun} -> dryRun) (\s@DescribeFpgaImageAttribute' {} a -> s {dryRun = a} :: DescribeFpgaImageAttribute)

-- | The ID of the AFI.
describeFpgaImageAttribute_fpgaImageId :: Lens.Lens' DescribeFpgaImageAttribute Prelude.Text
describeFpgaImageAttribute_fpgaImageId = Lens.lens (\DescribeFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@DescribeFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: DescribeFpgaImageAttribute)

-- | The AFI attribute.
describeFpgaImageAttribute_attribute :: Lens.Lens' DescribeFpgaImageAttribute FpgaImageAttributeName
describeFpgaImageAttribute_attribute = Lens.lens (\DescribeFpgaImageAttribute' {attribute} -> attribute) (\s@DescribeFpgaImageAttribute' {} a -> s {attribute = a} :: DescribeFpgaImageAttribute)

instance Core.AWSRequest DescribeFpgaImageAttribute where
  type
    AWSResponse DescribeFpgaImageAttribute =
      DescribeFpgaImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFpgaImageAttributeResponse'
            Prelude.<$> (x Data..@? "fpgaImageAttribute")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFpgaImageAttribute where
  hashWithSalt _salt DescribeFpgaImageAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` fpgaImageId
      `Prelude.hashWithSalt` attribute

instance Prelude.NFData DescribeFpgaImageAttribute where
  rnf DescribeFpgaImageAttribute' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf fpgaImageId
      `Prelude.seq` Prelude.rnf attribute

instance Data.ToHeaders DescribeFpgaImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFpgaImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFpgaImageAttribute where
  toQuery DescribeFpgaImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeFpgaImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "FpgaImageId" Data.=: fpgaImageId,
        "Attribute" Data.=: attribute
      ]

-- | /See:/ 'newDescribeFpgaImageAttributeResponse' smart constructor.
data DescribeFpgaImageAttributeResponse = DescribeFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Prelude.Maybe FpgaImageAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFpgaImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fpgaImageAttribute', 'describeFpgaImageAttributeResponse_fpgaImageAttribute' - Information about the attribute.
--
-- 'httpStatus', 'describeFpgaImageAttributeResponse_httpStatus' - The response's http status code.
newDescribeFpgaImageAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFpgaImageAttributeResponse
newDescribeFpgaImageAttributeResponse pHttpStatus_ =
  DescribeFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attribute.
describeFpgaImageAttributeResponse_fpgaImageAttribute :: Lens.Lens' DescribeFpgaImageAttributeResponse (Prelude.Maybe FpgaImageAttribute)
describeFpgaImageAttributeResponse_fpgaImageAttribute = Lens.lens (\DescribeFpgaImageAttributeResponse' {fpgaImageAttribute} -> fpgaImageAttribute) (\s@DescribeFpgaImageAttributeResponse' {} a -> s {fpgaImageAttribute = a} :: DescribeFpgaImageAttributeResponse)

-- | The response's http status code.
describeFpgaImageAttributeResponse_httpStatus :: Lens.Lens' DescribeFpgaImageAttributeResponse Prelude.Int
describeFpgaImageAttributeResponse_httpStatus = Lens.lens (\DescribeFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: DescribeFpgaImageAttributeResponse)

instance
  Prelude.NFData
    DescribeFpgaImageAttributeResponse
  where
  rnf DescribeFpgaImageAttributeResponse' {..} =
    Prelude.rnf fpgaImageAttribute
      `Prelude.seq` Prelude.rnf httpStatus
