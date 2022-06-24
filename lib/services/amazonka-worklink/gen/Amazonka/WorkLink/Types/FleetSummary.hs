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
-- Module      : Amazonka.WorkLink.Types.FleetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.FleetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkLink.Types.FleetStatus

-- | The summary of the fleet.
--
-- /See:/ 'newFleetSummary' smart constructor.
data FleetSummary = FleetSummary'
  { -- | The tags attached to the resource. A tag is a key-value pair.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier used by users to sign into the Amazon WorkLink app.
    companyCode :: Prelude.Maybe Prelude.Text,
    -- | The time when the fleet was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The time when the fleet was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet.
    fleetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the fleet.
    fleetStatus :: Prelude.Maybe FleetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'fleetSummary_tags' - The tags attached to the resource. A tag is a key-value pair.
--
-- 'companyCode', 'fleetSummary_companyCode' - The identifier used by users to sign into the Amazon WorkLink app.
--
-- 'createdTime', 'fleetSummary_createdTime' - The time when the fleet was created.
--
-- 'displayName', 'fleetSummary_displayName' - The name of the fleet to display.
--
-- 'lastUpdatedTime', 'fleetSummary_lastUpdatedTime' - The time when the fleet was last updated.
--
-- 'fleetName', 'fleetSummary_fleetName' - The name of the fleet.
--
-- 'fleetArn', 'fleetSummary_fleetArn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'fleetStatus', 'fleetSummary_fleetStatus' - The status of the fleet.
newFleetSummary ::
  FleetSummary
newFleetSummary =
  FleetSummary'
    { tags = Prelude.Nothing,
      companyCode = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      fleetName = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetStatus = Prelude.Nothing
    }

-- | The tags attached to the resource. A tag is a key-value pair.
fleetSummary_tags :: Lens.Lens' FleetSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
fleetSummary_tags = Lens.lens (\FleetSummary' {tags} -> tags) (\s@FleetSummary' {} a -> s {tags = a} :: FleetSummary) Prelude.. Lens.mapping Lens.coerced

-- | The identifier used by users to sign into the Amazon WorkLink app.
fleetSummary_companyCode :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.Text)
fleetSummary_companyCode = Lens.lens (\FleetSummary' {companyCode} -> companyCode) (\s@FleetSummary' {} a -> s {companyCode = a} :: FleetSummary)

-- | The time when the fleet was created.
fleetSummary_createdTime :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.UTCTime)
fleetSummary_createdTime = Lens.lens (\FleetSummary' {createdTime} -> createdTime) (\s@FleetSummary' {} a -> s {createdTime = a} :: FleetSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet to display.
fleetSummary_displayName :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.Text)
fleetSummary_displayName = Lens.lens (\FleetSummary' {displayName} -> displayName) (\s@FleetSummary' {} a -> s {displayName = a} :: FleetSummary)

-- | The time when the fleet was last updated.
fleetSummary_lastUpdatedTime :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.UTCTime)
fleetSummary_lastUpdatedTime = Lens.lens (\FleetSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@FleetSummary' {} a -> s {lastUpdatedTime = a} :: FleetSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet.
fleetSummary_fleetName :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.Text)
fleetSummary_fleetName = Lens.lens (\FleetSummary' {fleetName} -> fleetName) (\s@FleetSummary' {} a -> s {fleetName = a} :: FleetSummary)

-- | The Amazon Resource Name (ARN) of the fleet.
fleetSummary_fleetArn :: Lens.Lens' FleetSummary (Prelude.Maybe Prelude.Text)
fleetSummary_fleetArn = Lens.lens (\FleetSummary' {fleetArn} -> fleetArn) (\s@FleetSummary' {} a -> s {fleetArn = a} :: FleetSummary)

-- | The status of the fleet.
fleetSummary_fleetStatus :: Lens.Lens' FleetSummary (Prelude.Maybe FleetStatus)
fleetSummary_fleetStatus = Lens.lens (\FleetSummary' {fleetStatus} -> fleetStatus) (\s@FleetSummary' {} a -> s {fleetStatus = a} :: FleetSummary)

instance Core.FromJSON FleetSummary where
  parseJSON =
    Core.withObject
      "FleetSummary"
      ( \x ->
          FleetSummary'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CompanyCode")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "FleetName")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "FleetStatus")
      )

instance Prelude.Hashable FleetSummary where
  hashWithSalt _salt FleetSummary' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` companyCode
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetStatus

instance Prelude.NFData FleetSummary where
  rnf FleetSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf companyCode
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetStatus
