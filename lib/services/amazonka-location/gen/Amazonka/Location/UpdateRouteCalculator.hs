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
-- Module      : Amazonka.Location.UpdateRouteCalculator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties for a given route calculator resource.
module Amazonka.Location.UpdateRouteCalculator
  ( -- * Creating a Request
    UpdateRouteCalculator (..),
    newUpdateRouteCalculator,

    -- * Request Lenses
    updateRouteCalculator_description,
    updateRouteCalculator_pricingPlan,
    updateRouteCalculator_calculatorName,

    -- * Destructuring the Response
    UpdateRouteCalculatorResponse (..),
    newUpdateRouteCalculatorResponse,

    -- * Response Lenses
    updateRouteCalculatorResponse_httpStatus,
    updateRouteCalculatorResponse_calculatorArn,
    updateRouteCalculatorResponse_calculatorName,
    updateRouteCalculatorResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRouteCalculator' smart constructor.
data UpdateRouteCalculator = UpdateRouteCalculator'
  { -- | Updates the description for the route calculator resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The name of the route calculator resource to update.
    calculatorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteCalculator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRouteCalculator_description' - Updates the description for the route calculator resource.
--
-- 'pricingPlan', 'updateRouteCalculator_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'calculatorName', 'updateRouteCalculator_calculatorName' - The name of the route calculator resource to update.
newUpdateRouteCalculator ::
  -- | 'calculatorName'
  Prelude.Text ->
  UpdateRouteCalculator
newUpdateRouteCalculator pCalculatorName_ =
  UpdateRouteCalculator'
    { description =
        Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      calculatorName = pCalculatorName_
    }

-- | Updates the description for the route calculator resource.
updateRouteCalculator_description :: Lens.Lens' UpdateRouteCalculator (Prelude.Maybe Prelude.Text)
updateRouteCalculator_description = Lens.lens (\UpdateRouteCalculator' {description} -> description) (\s@UpdateRouteCalculator' {} a -> s {description = a} :: UpdateRouteCalculator)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
updateRouteCalculator_pricingPlan :: Lens.Lens' UpdateRouteCalculator (Prelude.Maybe PricingPlan)
updateRouteCalculator_pricingPlan = Lens.lens (\UpdateRouteCalculator' {pricingPlan} -> pricingPlan) (\s@UpdateRouteCalculator' {} a -> s {pricingPlan = a} :: UpdateRouteCalculator)

-- | The name of the route calculator resource to update.
updateRouteCalculator_calculatorName :: Lens.Lens' UpdateRouteCalculator Prelude.Text
updateRouteCalculator_calculatorName = Lens.lens (\UpdateRouteCalculator' {calculatorName} -> calculatorName) (\s@UpdateRouteCalculator' {} a -> s {calculatorName = a} :: UpdateRouteCalculator)

instance Core.AWSRequest UpdateRouteCalculator where
  type
    AWSResponse UpdateRouteCalculator =
      UpdateRouteCalculatorResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRouteCalculatorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CalculatorArn")
            Prelude.<*> (x Core..:> "CalculatorName")
            Prelude.<*> (x Core..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateRouteCalculator where
  hashWithSalt _salt UpdateRouteCalculator' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` calculatorName

instance Prelude.NFData UpdateRouteCalculator where
  rnf UpdateRouteCalculator' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf calculatorName

instance Core.ToHeaders UpdateRouteCalculator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRouteCalculator where
  toJSON UpdateRouteCalculator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            ("PricingPlan" Core..=) Prelude.<$> pricingPlan
          ]
      )

instance Core.ToPath UpdateRouteCalculator where
  toPath UpdateRouteCalculator' {..} =
    Prelude.mconcat
      ["/routes/v0/calculators/", Core.toBS calculatorName]

instance Core.ToQuery UpdateRouteCalculator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRouteCalculatorResponse' smart constructor.
data UpdateRouteCalculatorResponse = UpdateRouteCalculatorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated route calculator resource.
    -- Used to specify a resource across AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:route- calculator\/ExampleCalculator@
    calculatorArn :: Prelude.Text,
    -- | The name of the updated route calculator resource.
    calculatorName :: Prelude.Text,
    -- | The timestamp for when the route calculator was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteCalculatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRouteCalculatorResponse_httpStatus' - The response's http status code.
--
-- 'calculatorArn', 'updateRouteCalculatorResponse_calculatorArn' - The Amazon Resource Name (ARN) of the updated route calculator resource.
-- Used to specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route- calculator\/ExampleCalculator@
--
-- 'calculatorName', 'updateRouteCalculatorResponse_calculatorName' - The name of the updated route calculator resource.
--
-- 'updateTime', 'updateRouteCalculatorResponse_updateTime' - The timestamp for when the route calculator was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newUpdateRouteCalculatorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'calculatorArn'
  Prelude.Text ->
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateRouteCalculatorResponse
newUpdateRouteCalculatorResponse
  pHttpStatus_
  pCalculatorArn_
  pCalculatorName_
  pUpdateTime_ =
    UpdateRouteCalculatorResponse'
      { httpStatus =
          pHttpStatus_,
        calculatorArn = pCalculatorArn_,
        calculatorName = pCalculatorName_,
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateRouteCalculatorResponse_httpStatus :: Lens.Lens' UpdateRouteCalculatorResponse Prelude.Int
updateRouteCalculatorResponse_httpStatus = Lens.lens (\UpdateRouteCalculatorResponse' {httpStatus} -> httpStatus) (\s@UpdateRouteCalculatorResponse' {} a -> s {httpStatus = a} :: UpdateRouteCalculatorResponse)

-- | The Amazon Resource Name (ARN) of the updated route calculator resource.
-- Used to specify a resource across AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:route- calculator\/ExampleCalculator@
updateRouteCalculatorResponse_calculatorArn :: Lens.Lens' UpdateRouteCalculatorResponse Prelude.Text
updateRouteCalculatorResponse_calculatorArn = Lens.lens (\UpdateRouteCalculatorResponse' {calculatorArn} -> calculatorArn) (\s@UpdateRouteCalculatorResponse' {} a -> s {calculatorArn = a} :: UpdateRouteCalculatorResponse)

-- | The name of the updated route calculator resource.
updateRouteCalculatorResponse_calculatorName :: Lens.Lens' UpdateRouteCalculatorResponse Prelude.Text
updateRouteCalculatorResponse_calculatorName = Lens.lens (\UpdateRouteCalculatorResponse' {calculatorName} -> calculatorName) (\s@UpdateRouteCalculatorResponse' {} a -> s {calculatorName = a} :: UpdateRouteCalculatorResponse)

-- | The timestamp for when the route calculator was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateRouteCalculatorResponse_updateTime :: Lens.Lens' UpdateRouteCalculatorResponse Prelude.UTCTime
updateRouteCalculatorResponse_updateTime = Lens.lens (\UpdateRouteCalculatorResponse' {updateTime} -> updateTime) (\s@UpdateRouteCalculatorResponse' {} a -> s {updateTime = a} :: UpdateRouteCalculatorResponse) Prelude.. Core._Time

instance Prelude.NFData UpdateRouteCalculatorResponse where
  rnf UpdateRouteCalculatorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf calculatorArn
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf updateTime
