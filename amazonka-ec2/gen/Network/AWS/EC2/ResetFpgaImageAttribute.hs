{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The attribute.
    attribute :: Prelude.Maybe ResetFpgaImageAttributeName,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ResetFpgaImageAttribute
newResetFpgaImageAttribute pFpgaImageId_ =
  ResetFpgaImageAttribute'
    { dryRun = Prelude.Nothing,
      attribute = Prelude.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetFpgaImageAttribute_dryRun :: Lens.Lens' ResetFpgaImageAttribute (Prelude.Maybe Prelude.Bool)
resetFpgaImageAttribute_dryRun = Lens.lens (\ResetFpgaImageAttribute' {dryRun} -> dryRun) (\s@ResetFpgaImageAttribute' {} a -> s {dryRun = a} :: ResetFpgaImageAttribute)

-- | The attribute.
resetFpgaImageAttribute_attribute :: Lens.Lens' ResetFpgaImageAttribute (Prelude.Maybe ResetFpgaImageAttributeName)
resetFpgaImageAttribute_attribute = Lens.lens (\ResetFpgaImageAttribute' {attribute} -> attribute) (\s@ResetFpgaImageAttribute' {} a -> s {attribute = a} :: ResetFpgaImageAttribute)

-- | The ID of the AFI.
resetFpgaImageAttribute_fpgaImageId :: Lens.Lens' ResetFpgaImageAttribute Prelude.Text
resetFpgaImageAttribute_fpgaImageId = Lens.lens (\ResetFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@ResetFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: ResetFpgaImageAttribute)

instance Prelude.AWSRequest ResetFpgaImageAttribute where
  type
    Rs ResetFpgaImageAttribute =
      ResetFpgaImageAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ResetFpgaImageAttributeResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetFpgaImageAttribute

instance Prelude.NFData ResetFpgaImageAttribute

instance Prelude.ToHeaders ResetFpgaImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ResetFpgaImageAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetFpgaImageAttribute where
  toQuery ResetFpgaImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ResetFpgaImageAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "Attribute" Prelude.=: attribute,
        "FpgaImageId" Prelude.=: fpgaImageId
      ]

-- | /See:/ 'newResetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ResetFpgaImageAttributeResponse
newResetFpgaImageAttributeResponse pHttpStatus_ =
  ResetFpgaImageAttributeResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
resetFpgaImageAttributeResponse_return :: Lens.Lens' ResetFpgaImageAttributeResponse (Prelude.Maybe Prelude.Bool)
resetFpgaImageAttributeResponse_return = Lens.lens (\ResetFpgaImageAttributeResponse' {return'} -> return') (\s@ResetFpgaImageAttributeResponse' {} a -> s {return' = a} :: ResetFpgaImageAttributeResponse)

-- | The response's http status code.
resetFpgaImageAttributeResponse_httpStatus :: Lens.Lens' ResetFpgaImageAttributeResponse Prelude.Int
resetFpgaImageAttributeResponse_httpStatus = Lens.lens (\ResetFpgaImageAttributeResponse' {httpStatus} -> httpStatus) (\s@ResetFpgaImageAttributeResponse' {} a -> s {httpStatus = a} :: ResetFpgaImageAttributeResponse)

instance
  Prelude.NFData
    ResetFpgaImageAttributeResponse
