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
-- Module      : Amazonka.GuardDuty.Types.TriggerDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.TriggerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the reason the scan was triggered.
--
-- /See:/ 'newTriggerDetails' smart constructor.
data TriggerDetails = TriggerDetails'
  { -- | The description of the scan trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the GuardDuty finding that triggered the BirdDog scan.
    guardDutyFindingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'triggerDetails_description' - The description of the scan trigger.
--
-- 'guardDutyFindingId', 'triggerDetails_guardDutyFindingId' - The ID of the GuardDuty finding that triggered the BirdDog scan.
newTriggerDetails ::
  TriggerDetails
newTriggerDetails =
  TriggerDetails'
    { description = Prelude.Nothing,
      guardDutyFindingId = Prelude.Nothing
    }

-- | The description of the scan trigger.
triggerDetails_description :: Lens.Lens' TriggerDetails (Prelude.Maybe Prelude.Text)
triggerDetails_description = Lens.lens (\TriggerDetails' {description} -> description) (\s@TriggerDetails' {} a -> s {description = a} :: TriggerDetails)

-- | The ID of the GuardDuty finding that triggered the BirdDog scan.
triggerDetails_guardDutyFindingId :: Lens.Lens' TriggerDetails (Prelude.Maybe Prelude.Text)
triggerDetails_guardDutyFindingId = Lens.lens (\TriggerDetails' {guardDutyFindingId} -> guardDutyFindingId) (\s@TriggerDetails' {} a -> s {guardDutyFindingId = a} :: TriggerDetails)

instance Data.FromJSON TriggerDetails where
  parseJSON =
    Data.withObject
      "TriggerDetails"
      ( \x ->
          TriggerDetails'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "guardDutyFindingId")
      )

instance Prelude.Hashable TriggerDetails where
  hashWithSalt _salt TriggerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` guardDutyFindingId

instance Prelude.NFData TriggerDetails where
  rnf TriggerDetails' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf guardDutyFindingId
