{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.InstanceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.InstanceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an EC2 instance providing user-based subscriptions.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | The date of the last status check.
    lastStatusCheckDate :: Prelude.Maybe Prelude.Text,
    -- | The status message for an EC2 instance.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 instance, which provides user-based subscriptions.
    instanceId :: Prelude.Text,
    -- | A list of provided user-based subscription products.
    products :: [Prelude.Text],
    -- | The status of an EC2 instance resource.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastStatusCheckDate', 'instanceSummary_lastStatusCheckDate' - The date of the last status check.
--
-- 'statusMessage', 'instanceSummary_statusMessage' - The status message for an EC2 instance.
--
-- 'instanceId', 'instanceSummary_instanceId' - The ID of the EC2 instance, which provides user-based subscriptions.
--
-- 'products', 'instanceSummary_products' - A list of provided user-based subscription products.
--
-- 'status', 'instanceSummary_status' - The status of an EC2 instance resource.
newInstanceSummary ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  InstanceSummary
newInstanceSummary pInstanceId_ pStatus_ =
  InstanceSummary'
    { lastStatusCheckDate =
        Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      instanceId = pInstanceId_,
      products = Prelude.mempty,
      status = pStatus_
    }

-- | The date of the last status check.
instanceSummary_lastStatusCheckDate :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_lastStatusCheckDate = Lens.lens (\InstanceSummary' {lastStatusCheckDate} -> lastStatusCheckDate) (\s@InstanceSummary' {} a -> s {lastStatusCheckDate = a} :: InstanceSummary)

-- | The status message for an EC2 instance.
instanceSummary_statusMessage :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_statusMessage = Lens.lens (\InstanceSummary' {statusMessage} -> statusMessage) (\s@InstanceSummary' {} a -> s {statusMessage = a} :: InstanceSummary)

-- | The ID of the EC2 instance, which provides user-based subscriptions.
instanceSummary_instanceId :: Lens.Lens' InstanceSummary Prelude.Text
instanceSummary_instanceId = Lens.lens (\InstanceSummary' {instanceId} -> instanceId) (\s@InstanceSummary' {} a -> s {instanceId = a} :: InstanceSummary)

-- | A list of provided user-based subscription products.
instanceSummary_products :: Lens.Lens' InstanceSummary [Prelude.Text]
instanceSummary_products = Lens.lens (\InstanceSummary' {products} -> products) (\s@InstanceSummary' {} a -> s {products = a} :: InstanceSummary) Prelude.. Lens.coerced

-- | The status of an EC2 instance resource.
instanceSummary_status :: Lens.Lens' InstanceSummary Prelude.Text
instanceSummary_status = Lens.lens (\InstanceSummary' {status} -> status) (\s@InstanceSummary' {} a -> s {status = a} :: InstanceSummary)

instance Core.FromJSON InstanceSummary where
  parseJSON =
    Core.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Core..:? "LastStatusCheckDate")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..: "InstanceId")
            Prelude.<*> (x Core..:? "Products" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable InstanceSummary where
  hashWithSalt _salt InstanceSummary' {..} =
    _salt `Prelude.hashWithSalt` lastStatusCheckDate
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` products
      `Prelude.hashWithSalt` status

instance Prelude.NFData InstanceSummary where
  rnf InstanceSummary' {..} =
    Prelude.rnf lastStatusCheckDate
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf products
      `Prelude.seq` Prelude.rnf status
