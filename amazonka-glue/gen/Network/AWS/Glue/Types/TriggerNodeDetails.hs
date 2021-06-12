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
-- Module      : Network.AWS.Glue.Types.TriggerNodeDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerNodeDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.Trigger
import qualified Network.AWS.Lens as Lens

-- | The details of a Trigger node present in the workflow.
--
-- /See:/ 'newTriggerNodeDetails' smart constructor.
data TriggerNodeDetails = TriggerNodeDetails'
  { -- | The information of the trigger represented by the trigger node.
    trigger :: Core.Maybe Trigger
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TriggerNodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trigger', 'triggerNodeDetails_trigger' - The information of the trigger represented by the trigger node.
newTriggerNodeDetails ::
  TriggerNodeDetails
newTriggerNodeDetails =
  TriggerNodeDetails' {trigger = Core.Nothing}

-- | The information of the trigger represented by the trigger node.
triggerNodeDetails_trigger :: Lens.Lens' TriggerNodeDetails (Core.Maybe Trigger)
triggerNodeDetails_trigger = Lens.lens (\TriggerNodeDetails' {trigger} -> trigger) (\s@TriggerNodeDetails' {} a -> s {trigger = a} :: TriggerNodeDetails)

instance Core.FromJSON TriggerNodeDetails where
  parseJSON =
    Core.withObject
      "TriggerNodeDetails"
      ( \x ->
          TriggerNodeDetails' Core.<$> (x Core..:? "Trigger")
      )

instance Core.Hashable TriggerNodeDetails

instance Core.NFData TriggerNodeDetails
