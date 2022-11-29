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
-- Module      : Amazonka.SavingsPlans.CreateSavingsPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Savings Plan.
module Amazonka.SavingsPlans.CreateSavingsPlan
  ( -- * Creating a Request
    CreateSavingsPlan (..),
    newCreateSavingsPlan,

    -- * Request Lenses
    createSavingsPlan_tags,
    createSavingsPlan_upfrontPaymentAmount,
    createSavingsPlan_clientToken,
    createSavingsPlan_purchaseTime,
    createSavingsPlan_savingsPlanOfferingId,
    createSavingsPlan_commitment,

    -- * Destructuring the Response
    CreateSavingsPlanResponse (..),
    newCreateSavingsPlanResponse,

    -- * Response Lenses
    createSavingsPlanResponse_savingsPlanId,
    createSavingsPlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newCreateSavingsPlan' smart constructor.
data CreateSavingsPlan = CreateSavingsPlan'
  { -- | One or more tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The up-front payment amount. This is a whole number between 50 and 99
    -- percent of the total value of the Savings Plan. This parameter is
    -- supported only if the payment option is @Partial Upfront@.
    upfrontPaymentAmount :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The time at which to purchase the Savings Plan, in UTC format
    -- (YYYY-MM-DDTHH:MM:SSZ).
    purchaseTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the offering.
    savingsPlanOfferingId :: Prelude.Text,
    -- | The hourly commitment, in USD. This is a value between 0.001 and 1
    -- million. You cannot specify more than five digits after the decimal
    -- point.
    commitment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSavingsPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSavingsPlan_tags' - One or more tags.
--
-- 'upfrontPaymentAmount', 'createSavingsPlan_upfrontPaymentAmount' - The up-front payment amount. This is a whole number between 50 and 99
-- percent of the total value of the Savings Plan. This parameter is
-- supported only if the payment option is @Partial Upfront@.
--
-- 'clientToken', 'createSavingsPlan_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'purchaseTime', 'createSavingsPlan_purchaseTime' - The time at which to purchase the Savings Plan, in UTC format
-- (YYYY-MM-DDTHH:MM:SSZ).
--
-- 'savingsPlanOfferingId', 'createSavingsPlan_savingsPlanOfferingId' - The ID of the offering.
--
-- 'commitment', 'createSavingsPlan_commitment' - The hourly commitment, in USD. This is a value between 0.001 and 1
-- million. You cannot specify more than five digits after the decimal
-- point.
newCreateSavingsPlan ::
  -- | 'savingsPlanOfferingId'
  Prelude.Text ->
  -- | 'commitment'
  Prelude.Text ->
  CreateSavingsPlan
newCreateSavingsPlan
  pSavingsPlanOfferingId_
  pCommitment_ =
    CreateSavingsPlan'
      { tags = Prelude.Nothing,
        upfrontPaymentAmount = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        purchaseTime = Prelude.Nothing,
        savingsPlanOfferingId = pSavingsPlanOfferingId_,
        commitment = pCommitment_
      }

-- | One or more tags.
createSavingsPlan_tags :: Lens.Lens' CreateSavingsPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSavingsPlan_tags = Lens.lens (\CreateSavingsPlan' {tags} -> tags) (\s@CreateSavingsPlan' {} a -> s {tags = a} :: CreateSavingsPlan) Prelude.. Lens.mapping Lens.coerced

-- | The up-front payment amount. This is a whole number between 50 and 99
-- percent of the total value of the Savings Plan. This parameter is
-- supported only if the payment option is @Partial Upfront@.
createSavingsPlan_upfrontPaymentAmount :: Lens.Lens' CreateSavingsPlan (Prelude.Maybe Prelude.Text)
createSavingsPlan_upfrontPaymentAmount = Lens.lens (\CreateSavingsPlan' {upfrontPaymentAmount} -> upfrontPaymentAmount) (\s@CreateSavingsPlan' {} a -> s {upfrontPaymentAmount = a} :: CreateSavingsPlan)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createSavingsPlan_clientToken :: Lens.Lens' CreateSavingsPlan (Prelude.Maybe Prelude.Text)
createSavingsPlan_clientToken = Lens.lens (\CreateSavingsPlan' {clientToken} -> clientToken) (\s@CreateSavingsPlan' {} a -> s {clientToken = a} :: CreateSavingsPlan)

-- | The time at which to purchase the Savings Plan, in UTC format
-- (YYYY-MM-DDTHH:MM:SSZ).
createSavingsPlan_purchaseTime :: Lens.Lens' CreateSavingsPlan (Prelude.Maybe Prelude.UTCTime)
createSavingsPlan_purchaseTime = Lens.lens (\CreateSavingsPlan' {purchaseTime} -> purchaseTime) (\s@CreateSavingsPlan' {} a -> s {purchaseTime = a} :: CreateSavingsPlan) Prelude.. Lens.mapping Core._Time

-- | The ID of the offering.
createSavingsPlan_savingsPlanOfferingId :: Lens.Lens' CreateSavingsPlan Prelude.Text
createSavingsPlan_savingsPlanOfferingId = Lens.lens (\CreateSavingsPlan' {savingsPlanOfferingId} -> savingsPlanOfferingId) (\s@CreateSavingsPlan' {} a -> s {savingsPlanOfferingId = a} :: CreateSavingsPlan)

-- | The hourly commitment, in USD. This is a value between 0.001 and 1
-- million. You cannot specify more than five digits after the decimal
-- point.
createSavingsPlan_commitment :: Lens.Lens' CreateSavingsPlan Prelude.Text
createSavingsPlan_commitment = Lens.lens (\CreateSavingsPlan' {commitment} -> commitment) (\s@CreateSavingsPlan' {} a -> s {commitment = a} :: CreateSavingsPlan)

instance Core.AWSRequest CreateSavingsPlan where
  type
    AWSResponse CreateSavingsPlan =
      CreateSavingsPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSavingsPlanResponse'
            Prelude.<$> (x Core..?> "savingsPlanId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSavingsPlan where
  hashWithSalt _salt CreateSavingsPlan' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` upfrontPaymentAmount
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` purchaseTime
      `Prelude.hashWithSalt` savingsPlanOfferingId
      `Prelude.hashWithSalt` commitment

instance Prelude.NFData CreateSavingsPlan where
  rnf CreateSavingsPlan' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf upfrontPaymentAmount
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf purchaseTime
      `Prelude.seq` Prelude.rnf savingsPlanOfferingId
      `Prelude.seq` Prelude.rnf commitment

instance Core.ToHeaders CreateSavingsPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSavingsPlan where
  toJSON CreateSavingsPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("upfrontPaymentAmount" Core..=)
              Prelude.<$> upfrontPaymentAmount,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("purchaseTime" Core..=) Prelude.<$> purchaseTime,
            Prelude.Just
              ( "savingsPlanOfferingId"
                  Core..= savingsPlanOfferingId
              ),
            Prelude.Just ("commitment" Core..= commitment)
          ]
      )

instance Core.ToPath CreateSavingsPlan where
  toPath = Prelude.const "/CreateSavingsPlan"

instance Core.ToQuery CreateSavingsPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSavingsPlanResponse' smart constructor.
data CreateSavingsPlanResponse = CreateSavingsPlanResponse'
  { -- | The ID of the Savings Plan.
    savingsPlanId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSavingsPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'savingsPlanId', 'createSavingsPlanResponse_savingsPlanId' - The ID of the Savings Plan.
--
-- 'httpStatus', 'createSavingsPlanResponse_httpStatus' - The response's http status code.
newCreateSavingsPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSavingsPlanResponse
newCreateSavingsPlanResponse pHttpStatus_ =
  CreateSavingsPlanResponse'
    { savingsPlanId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Savings Plan.
createSavingsPlanResponse_savingsPlanId :: Lens.Lens' CreateSavingsPlanResponse (Prelude.Maybe Prelude.Text)
createSavingsPlanResponse_savingsPlanId = Lens.lens (\CreateSavingsPlanResponse' {savingsPlanId} -> savingsPlanId) (\s@CreateSavingsPlanResponse' {} a -> s {savingsPlanId = a} :: CreateSavingsPlanResponse)

-- | The response's http status code.
createSavingsPlanResponse_httpStatus :: Lens.Lens' CreateSavingsPlanResponse Prelude.Int
createSavingsPlanResponse_httpStatus = Lens.lens (\CreateSavingsPlanResponse' {httpStatus} -> httpStatus) (\s@CreateSavingsPlanResponse' {} a -> s {httpStatus = a} :: CreateSavingsPlanResponse)

instance Prelude.NFData CreateSavingsPlanResponse where
  rnf CreateSavingsPlanResponse' {..} =
    Prelude.rnf savingsPlanId
      `Prelude.seq` Prelude.rnf httpStatus
