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
-- Module      : Network.AWS.AppFlow.Types.TriggerProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.TriggerProperties where

import Network.AWS.AppFlow.Types.ScheduledTriggerProperties
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration details that control the trigger for a flow.
-- Currently, these settings only apply to the @Scheduled@ trigger type.
--
-- /See:/ 'newTriggerProperties' smart constructor.
data TriggerProperties = TriggerProperties'
  { -- | Specifies the configuration details of a schedule-triggered flow as
    -- defined by the user.
    scheduled :: Prelude.Maybe ScheduledTriggerProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduled', 'triggerProperties_scheduled' - Specifies the configuration details of a schedule-triggered flow as
-- defined by the user.
newTriggerProperties ::
  TriggerProperties
newTriggerProperties =
  TriggerProperties' {scheduled = Prelude.Nothing}

-- | Specifies the configuration details of a schedule-triggered flow as
-- defined by the user.
triggerProperties_scheduled :: Lens.Lens' TriggerProperties (Prelude.Maybe ScheduledTriggerProperties)
triggerProperties_scheduled = Lens.lens (\TriggerProperties' {scheduled} -> scheduled) (\s@TriggerProperties' {} a -> s {scheduled = a} :: TriggerProperties)

instance Core.FromJSON TriggerProperties where
  parseJSON =
    Core.withObject
      "TriggerProperties"
      ( \x ->
          TriggerProperties'
            Prelude.<$> (x Core..:? "Scheduled")
      )

instance Prelude.Hashable TriggerProperties

instance Prelude.NFData TriggerProperties

instance Core.ToJSON TriggerProperties where
  toJSON TriggerProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Scheduled" Core..=) Prelude.<$> scheduled]
      )
