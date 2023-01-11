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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job with the long-term usage option for a device. The
-- long-term usage is a 1-year or 3-year long-term pricing type for the
-- device. You are billed upfront, and Amazon Web Services provides
-- discounts for long-term pricing.
module Amazonka.Snowball.CreateLongTermPricing
  ( -- * Creating a Request
    CreateLongTermPricing (..),
    newCreateLongTermPricing,

    -- * Request Lenses
    createLongTermPricing_isLongTermPricingAutoRenew,
    createLongTermPricing_snowballType,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newCreateLongTermPricing' smart constructor.
data CreateLongTermPricing = CreateLongTermPricing'
  { -- | snowballty
    --
    -- Specifies whether the current long-term pricing type for the device
    -- should be renewed.
    isLongTermPricingAutoRenew :: Prelude.Maybe Prelude.Bool,
    -- | The type of Snow Family devices to use for the long-term pricing job.
    snowballType :: Prelude.Maybe SnowballType,
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
-- 'isLongTermPricingAutoRenew', 'createLongTermPricing_isLongTermPricingAutoRenew' - snowballty
--
-- Specifies whether the current long-term pricing type for the device
-- should be renewed.
--
-- 'snowballType', 'createLongTermPricing_snowballType' - The type of Snow Family devices to use for the long-term pricing job.
--
-- 'longTermPricingType', 'createLongTermPricing_longTermPricingType' - The type of long-term pricing option you want for the device, either
-- 1-year or 3-year long-term pricing.
newCreateLongTermPricing ::
  -- | 'longTermPricingType'
  LongTermPricingType ->
  CreateLongTermPricing
newCreateLongTermPricing pLongTermPricingType_ =
  CreateLongTermPricing'
    { isLongTermPricingAutoRenew =
        Prelude.Nothing,
      snowballType = Prelude.Nothing,
      longTermPricingType = pLongTermPricingType_
    }

-- | snowballty
--
-- Specifies whether the current long-term pricing type for the device
-- should be renewed.
createLongTermPricing_isLongTermPricingAutoRenew :: Lens.Lens' CreateLongTermPricing (Prelude.Maybe Prelude.Bool)
createLongTermPricing_isLongTermPricingAutoRenew = Lens.lens (\CreateLongTermPricing' {isLongTermPricingAutoRenew} -> isLongTermPricingAutoRenew) (\s@CreateLongTermPricing' {} a -> s {isLongTermPricingAutoRenew = a} :: CreateLongTermPricing)

-- | The type of Snow Family devices to use for the long-term pricing job.
createLongTermPricing_snowballType :: Lens.Lens' CreateLongTermPricing (Prelude.Maybe SnowballType)
createLongTermPricing_snowballType = Lens.lens (\CreateLongTermPricing' {snowballType} -> snowballType) (\s@CreateLongTermPricing' {} a -> s {snowballType = a} :: CreateLongTermPricing)

-- | The type of long-term pricing option you want for the device, either
-- 1-year or 3-year long-term pricing.
createLongTermPricing_longTermPricingType :: Lens.Lens' CreateLongTermPricing LongTermPricingType
createLongTermPricing_longTermPricingType = Lens.lens (\CreateLongTermPricing' {longTermPricingType} -> longTermPricingType) (\s@CreateLongTermPricing' {} a -> s {longTermPricingType = a} :: CreateLongTermPricing)

instance Core.AWSRequest CreateLongTermPricing where
  type
    AWSResponse CreateLongTermPricing =
      CreateLongTermPricingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLongTermPricingResponse'
            Prelude.<$> (x Data..?> "LongTermPricingId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLongTermPricing where
  hashWithSalt _salt CreateLongTermPricing' {..} =
    _salt
      `Prelude.hashWithSalt` isLongTermPricingAutoRenew
      `Prelude.hashWithSalt` snowballType
      `Prelude.hashWithSalt` longTermPricingType

instance Prelude.NFData CreateLongTermPricing where
  rnf CreateLongTermPricing' {..} =
    Prelude.rnf isLongTermPricingAutoRenew
      `Prelude.seq` Prelude.rnf snowballType
      `Prelude.seq` Prelude.rnf longTermPricingType

instance Data.ToHeaders CreateLongTermPricing where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.CreateLongTermPricing" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLongTermPricing where
  toJSON CreateLongTermPricing' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsLongTermPricingAutoRenew" Data..=)
              Prelude.<$> isLongTermPricingAutoRenew,
            ("SnowballType" Data..=) Prelude.<$> snowballType,
            Prelude.Just
              ("LongTermPricingType" Data..= longTermPricingType)
          ]
      )

instance Data.ToPath CreateLongTermPricing where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLongTermPricing where
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
