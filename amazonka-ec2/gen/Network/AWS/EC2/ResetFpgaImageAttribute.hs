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
-- Module      : Network.AWS.EC2.ResetFpgaImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified attribute of the specified Amazon FPGA Image (AFI)
-- to its default value. You can only reset the load permission attribute.
module Network.AWS.EC2.ResetFpgaImageAttribute
  ( -- * Creating a Request
    ResetFpgaImageAttribute (..),
    newResetFpgaImageAttribute,

    -- * Request Lenses
    resetFpgaImageAttribute_dryRun,
    resetFpgaImageAttribute_attribute,
    resetFpgaImageAttribute_fpgaImageId,

    -- * Destructuring the Response
    ResetFpgaImageAttributeResponse (..),
    newResetFpgaImageAttributeResponse,

    -- * Response Lenses
    resetFpgaImageAttributeResponse_return,
    resetFpgaImageAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The attribute.
    attribute :: Core.Maybe ResetFpgaImageAttributeName,
    -- | The ID of the AFI.
    fpgaImageId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetFpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'resetFpgaImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'attribute', 'resetFpgaImageAttribute_attribute' - The attribute.
--
-- 'fpgaImageId', 'resetFpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newResetFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Core.Text ->
  ResetFpgaImageAttribute
newResetFpgaImageAttribute pFpgaImageId_ =
  ResetFpgaImageAttribute'
    { dryRun = Core.Nothing,
      attribute = Core.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetFpgaImageAttribute_dryRun :: Lens.Lens' ResetFpgaImageAttribute (Core.Maybe Core.Bool)
resetFpgaImageAttribute_dryRun = Lens.lens (\ResetFpgaImageAttribute' {dryRun} -> dryRun) (\s@ResetFpgaImageAttribute' {} a -> s {dryRun = a} :: ResetFpgaImageAttribute)

-- | The attribute.
resetFpgaImageAttribute_attribute :: Lens.Lens' ResetFpgaImageAttribute (Core.Maybe ResetFpgaImageAttributeName)
resetFpgaImageAttribute_attribute = Lens.lens (\ResetFpgaImageAttribute' {attribute} -> attribute) (\s@ResetFpgaImageAttribute' {} a -> s {attribute = a} :: ResetFpgaImageAttribute)

-- | The ID of the AFI.
resetFpgaImageAttribute_fpgaImageId :: Lens.Lens' ResetFpgaImageAttribute Core.Text
resetFpgaImageAttribute_fpgaImageId = Lens.lens (\ResetFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@ResetFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: ResetFpgaImageAttribute)

instance Core.AWSRequest ResetFpgaImageAttribute where
  type
    AWSResponse ResetFpgaImageAttribute =
      ResetFpgaImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ResetFpgaImageAttributeResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ResetFpgaImageAttribute

instance Core.NFData ResetFpgaImageAttribute

instance Core.ToHeaders ResetFpgaImageAttribute where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ResetFpgaImageAttribute where
  toPath = Core.const "/"

instance Core.ToQuery ResetFpgaImageAttribute where
  toQuery ResetFpgaImageAttribute' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ResetFpgaImageAttribute" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Attribute" Core.=: attribute,
        "FpgaImageId" Core.=: fpgaImageId
      ]

-- | /See:/ 'newResetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResetFpgaImageAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'resetFpgaImageAttributeResponse_return' - Is @true@ if the request succeeds, and an error otherwise.
--
-- 'httpStatus', 'resetFpgaImageAttributeResponse_httpStatus' - The response's http status code.
newResetFpgaImageAttributeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ResetFpgaImageAttributeResponse
newResetFpgaImageAttributeResponse pHttpStatus_ =
  ResetFpgaImageAttributeResponse'
    { return' =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
resetFpgaImageAttributeResponse_return :: Lens.Lens' ResetFpgaImageAttributeResponse (Core.Maybe Core.Bool)
resetFpgaImageAttributeResponse_return = Lens.lens (\ResetFpgaImageAttributeResponse' {return'} -> return') (\s@ResetFpgaImageAttributeResponse' {} a -> s {return' = a} :: ResetFpgaImageAttributeResponse)

-- | The response's http status code.
resetFpgaImageAttributeResponse_httpStatus :: Lens.Lens' ResetFpgaImageAttributeResponse Core.Int
resetFpgaImageAttributeResponse_httpStatus = Lens.lens (\ResetFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@ResetFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: ResetFpgaImageAttributeResponse)

instance Core.NFData ResetFpgaImageAttributeResponse
