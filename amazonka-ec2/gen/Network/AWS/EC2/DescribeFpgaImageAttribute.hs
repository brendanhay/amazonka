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
-- Module      : Network.AWS.EC2.DescribeFpgaImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified Amazon FPGA Image
-- (AFI).
module Network.AWS.EC2.DescribeFpgaImageAttribute
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFpgaImageAttribute' smart constructor.
data DescribeFpgaImageAttribute = DescribeFpgaImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the AFI.
    fpgaImageId :: Core.Text,
    -- | The AFI attribute.
    attribute :: FpgaImageAttributeName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'attribute'
  FpgaImageAttributeName ->
  DescribeFpgaImageAttribute
newDescribeFpgaImageAttribute
  pFpgaImageId_
  pAttribute_ =
    DescribeFpgaImageAttribute'
      { dryRun = Core.Nothing,
        fpgaImageId = pFpgaImageId_,
        attribute = pAttribute_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFpgaImageAttribute_dryRun :: Lens.Lens' DescribeFpgaImageAttribute (Core.Maybe Core.Bool)
describeFpgaImageAttribute_dryRun = Lens.lens (\DescribeFpgaImageAttribute' {dryRun} -> dryRun) (\s@DescribeFpgaImageAttribute' {} a -> s {dryRun = a} :: DescribeFpgaImageAttribute)

-- | The ID of the AFI.
describeFpgaImageAttribute_fpgaImageId :: Lens.Lens' DescribeFpgaImageAttribute Core.Text
describeFpgaImageAttribute_fpgaImageId = Lens.lens (\DescribeFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@DescribeFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: DescribeFpgaImageAttribute)

-- | The AFI attribute.
describeFpgaImageAttribute_attribute :: Lens.Lens' DescribeFpgaImageAttribute FpgaImageAttributeName
describeFpgaImageAttribute_attribute = Lens.lens (\DescribeFpgaImageAttribute' {attribute} -> attribute) (\s@DescribeFpgaImageAttribute' {} a -> s {attribute = a} :: DescribeFpgaImageAttribute)

instance Core.AWSRequest DescribeFpgaImageAttribute where
  type
    AWSResponse DescribeFpgaImageAttribute =
      DescribeFpgaImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFpgaImageAttributeResponse'
            Core.<$> (x Core..@? "fpgaImageAttribute")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFpgaImageAttribute

instance Core.NFData DescribeFpgaImageAttribute

instance Core.ToHeaders DescribeFpgaImageAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeFpgaImageAttribute where
  toPath = Core.const "/"

instance Core.ToQuery DescribeFpgaImageAttribute where
  toQuery DescribeFpgaImageAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeFpgaImageAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "FpgaImageId" Core.=: fpgaImageId,
        "Attribute" Core.=: attribute
      ]

-- | /See:/ 'newDescribeFpgaImageAttributeResponse' smart constructor.
data DescribeFpgaImageAttributeResponse = DescribeFpgaImageAttributeResponse'
  { -- | Information about the attribute.
    fpgaImageAttribute :: Core.Maybe FpgaImageAttribute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeFpgaImageAttributeResponse
newDescribeFpgaImageAttributeResponse pHttpStatus_ =
  DescribeFpgaImageAttributeResponse'
    { fpgaImageAttribute =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attribute.
describeFpgaImageAttributeResponse_fpgaImageAttribute :: Lens.Lens' DescribeFpgaImageAttributeResponse (Core.Maybe FpgaImageAttribute)
describeFpgaImageAttributeResponse_fpgaImageAttribute = Lens.lens (\DescribeFpgaImageAttributeResponse' {fpgaImageAttribute} -> fpgaImageAttribute) (\s@DescribeFpgaImageAttributeResponse' {} a -> s {fpgaImageAttribute = a} :: DescribeFpgaImageAttributeResponse)

-- | The response's http status code.
describeFpgaImageAttributeResponse_httpStatus :: Lens.Lens' DescribeFpgaImageAttributeResponse Core.Int
describeFpgaImageAttributeResponse_httpStatus = Lens.lens (\DescribeFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@DescribeFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: DescribeFpgaImageAttributeResponse)

instance
  Core.NFData
    DescribeFpgaImageAttributeResponse
