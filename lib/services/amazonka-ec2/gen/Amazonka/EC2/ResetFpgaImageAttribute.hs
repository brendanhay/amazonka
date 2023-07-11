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
-- Module      : Amazonka.EC2.ResetFpgaImageAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified attribute of the specified Amazon FPGA Image (AFI)
-- to its default value. You can only reset the load permission attribute.
module Amazonka.EC2.ResetFpgaImageAttribute
  ( -- * Creating a Request
    ResetFpgaImageAttribute (..),
    newResetFpgaImageAttribute,

    -- * Request Lenses
    resetFpgaImageAttribute_attribute,
    resetFpgaImageAttribute_dryRun,
    resetFpgaImageAttribute_fpgaImageId,

    -- * Destructuring the Response
    ResetFpgaImageAttributeResponse (..),
    newResetFpgaImageAttributeResponse,

    -- * Response Lenses
    resetFpgaImageAttributeResponse_return,
    resetFpgaImageAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { -- | The attribute.
    attribute :: Prelude.Maybe ResetFpgaImageAttributeName,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetFpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'resetFpgaImageAttribute_attribute' - The attribute.
--
-- 'dryRun', 'resetFpgaImageAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'fpgaImageId', 'resetFpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newResetFpgaImageAttribute ::
  -- | 'fpgaImageId'
  Prelude.Text ->
  ResetFpgaImageAttribute
newResetFpgaImageAttribute pFpgaImageId_ =
  ResetFpgaImageAttribute'
    { attribute =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | The attribute.
resetFpgaImageAttribute_attribute :: Lens.Lens' ResetFpgaImageAttribute (Prelude.Maybe ResetFpgaImageAttributeName)
resetFpgaImageAttribute_attribute = Lens.lens (\ResetFpgaImageAttribute' {attribute} -> attribute) (\s@ResetFpgaImageAttribute' {} a -> s {attribute = a} :: ResetFpgaImageAttribute)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
resetFpgaImageAttribute_dryRun :: Lens.Lens' ResetFpgaImageAttribute (Prelude.Maybe Prelude.Bool)
resetFpgaImageAttribute_dryRun = Lens.lens (\ResetFpgaImageAttribute' {dryRun} -> dryRun) (\s@ResetFpgaImageAttribute' {} a -> s {dryRun = a} :: ResetFpgaImageAttribute)

-- | The ID of the AFI.
resetFpgaImageAttribute_fpgaImageId :: Lens.Lens' ResetFpgaImageAttribute Prelude.Text
resetFpgaImageAttribute_fpgaImageId = Lens.lens (\ResetFpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@ResetFpgaImageAttribute' {} a -> s {fpgaImageId = a} :: ResetFpgaImageAttribute)

instance Core.AWSRequest ResetFpgaImageAttribute where
  type
    AWSResponse ResetFpgaImageAttribute =
      ResetFpgaImageAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ResetFpgaImageAttributeResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetFpgaImageAttribute where
  hashWithSalt _salt ResetFpgaImageAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` fpgaImageId

instance Prelude.NFData ResetFpgaImageAttribute where
  rnf ResetFpgaImageAttribute' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf fpgaImageId

instance Data.ToHeaders ResetFpgaImageAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ResetFpgaImageAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetFpgaImageAttribute where
  toQuery ResetFpgaImageAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ResetFpgaImageAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Attribute" Data.=: attribute,
        "DryRun" Data.=: dryRun,
        "FpgaImageId" Data.=: fpgaImageId
      ]

-- | /See:/ 'newResetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf ResetFpgaImageAttributeResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
