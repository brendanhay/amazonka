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
-- Module      : Amazonka.Snowball.CreateLongTermPricing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job with the long-term usage option for a device. The
-- long-term usage is a 1-year or 3-year long-term pricing type for the
-- device. You are billed upfront, and AWS provides discounts for long-term
-- pricing.
module Amazonka.Snowball.CreateLongTermPricing
  ( -- * Creating a Request
    CreateLongTermPricing (..),
    newCreateLongTermPricing,

    -- * Request Lenses
    createLongTermPricing_snowballType,
    createLongTermPricing_isLongTermPricingAutoRenew,
    createLongTermPricing_longTermPricingType,

    -- * Destructuring the Response
    CreateLongTermPricingResponse (..),
    newCreateLongTermPricingResponse,

    -- * Response Lenses
    createLongTermPricingResponse_longTermPricingId,
    createLongTermPricingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCreateLongTermPricing' smart constructor.
data CreateLongTermPricing = CreateLongTermPricing'
  { -- | The type of AWS Snow Family device to use for the long-term pricing job.
    snowballType :: Prelude.Maybe SnowballType,
    -- | Specifies whether the current long-term pricing type for the device
    -- should be renewed.
    isLongTermPricingAutoRenew :: Prelude.Maybe Prelude.Bool,
    -- | The type of long-term pricing option you want for the device, either
    -- 1-year or 3-year long-term pricing.
    longTermPricingType :: LongTermPricingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLongTermPricing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowballType', 'createLongTermPricing_snowballType' - The type of AWS Snow Family device to use for the long-term pricing job.
--
-- 'isLongTermPricingAutoRenew', 'createLongTermPricing_isLongTermPricingAutoRenew' - Specifies whether the current long-term pricing type for the device
-- should be renewed.
--
-- 'longTermPricingType', 'createLongTermPricing_longTermPricingType' - The type of long-term pricing option you want for the device, either
-- 1-year or 3-year long-term pricing.
newCreateLongTermPricing ::
  -- | 'longTermPricingType'
  LongTermPricingType ->
  CreateLongTermPricing
newCreateLongTermPricing pLongTermPricingType_ =
  CreateLongTermPricing'
    { snowballType =
        Prelude.Nothing,
      isLongTermPricingAutoRenew = Prelude.Nothing,
      longTermPricingType = pLongTermPricingType_
    }

-- | The type of AWS Snow Family device to use for the long-term pricing job.
createLongTermPricing_snowballType :: Lens.Lens' CreateLongTermPricing (Prelude.Maybe SnowballType)
createLongTermPricing_snowballType = Lens.lens (\CreateLongTermPricing' {snowballType} -> snowballType) (\s@CreateLongTermPricing' {} a -> s {snowballType = a} :: CreateLongTermPricing)

-- | Specifies whether the current long-term pricing type for the device
-- should be renewed.
createLongTermPricing_isLongTermPricingAutoRenew :: Lens.Lens' CreateLongTermPricing (Prelude.Maybe Prelude.Bool)
createLongTermPricing_isLongTermPricingAutoRenew = Lens.lens (\CreateLongTermPricing' {isLongTermPricingAutoRenew} -> isLongTermPricingAutoRenew) (\s@CreateLongTermPricing' {} a -> s {isLongTermPricingAutoRenew = a} :: CreateLongTermPricing)

-- | The type of long-term pricing option you want for the device, either
-- 1-year or 3-year long-term pricing.
createLongTermPricing_longTermPricingType :: Lens.Lens' CreateLongTermPricing LongTermPricingType
createLongTermPricing_longTermPricingType = Lens.lens (\CreateLongTermPricing' {longTermPricingType} -> longTermPricingType) (\s@CreateLongTermPricing' {} a -> s {longTermPricingType = a} :: CreateLongTermPricing)

instance Core.AWSRequest CreateLongTermPricing where
  type
    AWSResponse CreateLongTermPricing =
      CreateLongTermPricingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLongTermPricingResponse'
            Prelude.<$> (x Core..?> "LongTermPricingId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLongTermPricing where
  hashWithSalt salt' CreateLongTermPricing' {..} =
    salt' `Prelude.hashWithSalt` longTermPricingType
      `Prelude.hashWithSalt` isLongTermPricingAutoRenew
      `Prelude.hashWithSalt` snowballType

instance Prelude.NFData CreateLongTermPricing where
  rnf CreateLongTermPricing' {..} =
    Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf longTermPricingType
      `Prelude.seq` Prelude.rnf isLongTermPricingAutoRenew

instance Core.ToHeaders CreateLongTermPricing where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.CreateLongTermPricing" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLongTermPricing where
  toJSON CreateLongTermPricing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SnowballType" Core..=) Prelude.<$> snowballType,
            ("IsLongTermPricingAutoRenew" Core..=)
              Prelude.<$> isLongTermPricingAutoRenew,
            Prelude.Just
              ("LongTermPricingType" Core..= longTermPricingType)
          ]
      )

instance Core.ToPath CreateLongTermPricing where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLongTermPricing where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLongTermPricingResponse' smart constructor.
data CreateLongTermPricingResponse = CreateLongTermPricingResponse'
  { -- | The ID of the long-term pricing type for the device.
    longTermPricingId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLongTermPricingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longTermPricingId', 'createLongTermPricingResponse_longTermPricingId' - The ID of the long-term pricing type for the device.
--
-- 'httpStatus', 'createLongTermPricingResponse_httpStatus' - The response's http status code.
newCreateLongTermPricingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLongTermPricingResponse
newCreateLongTermPricingResponse pHttpStatus_ =
  CreateLongTermPricingResponse'
    { longTermPricingId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the long-term pricing type for the device.
createLongTermPricingResponse_longTermPricingId :: Lens.Lens' CreateLongTermPricingResponse (Prelude.Maybe Prelude.Text)
createLongTermPricingResponse_longTermPricingId = Lens.lens (\CreateLongTermPricingResponse' {longTermPricingId} -> longTermPricingId) (\s@CreateLongTermPricingResponse' {} a -> s {longTermPricingId = a} :: CreateLongTermPricingResponse)

-- | The response's http status code.
createLongTermPricingResponse_httpStatus :: Lens.Lens' CreateLongTermPricingResponse Prelude.Int
createLongTermPricingResponse_httpStatus = Lens.lens (\CreateLongTermPricingResponse' {httpStatus} -> httpStatus) (\s@CreateLongTermPricingResponse' {} a -> s {httpStatus = a} :: CreateLongTermPricingResponse)

instance Prelude.NFData CreateLongTermPricingResponse where
  rnf CreateLongTermPricingResponse' {..} =
    Prelude.rnf longTermPricingId
      `Prelude.seq` Prelude.rnf httpStatus
