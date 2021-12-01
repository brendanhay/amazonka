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
-- Module      : Amazonka.EC2.Types.InstanceEventWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceEventWindow where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceEventWindowAssociationTarget
import Amazonka.EC2.Types.InstanceEventWindowState
import Amazonka.EC2.Types.InstanceEventWindowTimeRange
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The event window.
--
-- /See:/ 'newInstanceEventWindow' smart constructor.
data InstanceEventWindow = InstanceEventWindow'
  { -- | The current state of the event window.
    state :: Prelude.Maybe InstanceEventWindowState,
    -- | One or more targets associated with the event window.
    associationTarget :: Prelude.Maybe InstanceEventWindowAssociationTarget,
    -- | The ID of the event window.
    instanceEventWindowId :: Prelude.Maybe Prelude.Text,
    -- | The name of the event window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The cron expression defined for the event window.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The instance tags associated with the event window.
    tags :: Prelude.Maybe [Tag],
    -- | One or more time ranges defined for the event window.
    timeRanges :: Prelude.Maybe [InstanceEventWindowTimeRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'instanceEventWindow_state' - The current state of the event window.
--
-- 'associationTarget', 'instanceEventWindow_associationTarget' - One or more targets associated with the event window.
--
-- 'instanceEventWindowId', 'instanceEventWindow_instanceEventWindowId' - The ID of the event window.
--
-- 'name', 'instanceEventWindow_name' - The name of the event window.
--
-- 'cronExpression', 'instanceEventWindow_cronExpression' - The cron expression defined for the event window.
--
-- 'tags', 'instanceEventWindow_tags' - The instance tags associated with the event window.
--
-- 'timeRanges', 'instanceEventWindow_timeRanges' - One or more time ranges defined for the event window.
newInstanceEventWindow ::
  InstanceEventWindow
newInstanceEventWindow =
  InstanceEventWindow'
    { state = Prelude.Nothing,
      associationTarget = Prelude.Nothing,
      instanceEventWindowId = Prelude.Nothing,
      name = Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeRanges = Prelude.Nothing
    }

-- | The current state of the event window.
instanceEventWindow_state :: Lens.Lens' InstanceEventWindow (Prelude.Maybe InstanceEventWindowState)
instanceEventWindow_state = Lens.lens (\InstanceEventWindow' {state} -> state) (\s@InstanceEventWindow' {} a -> s {state = a} :: InstanceEventWindow)

-- | One or more targets associated with the event window.
instanceEventWindow_associationTarget :: Lens.Lens' InstanceEventWindow (Prelude.Maybe InstanceEventWindowAssociationTarget)
instanceEventWindow_associationTarget = Lens.lens (\InstanceEventWindow' {associationTarget} -> associationTarget) (\s@InstanceEventWindow' {} a -> s {associationTarget = a} :: InstanceEventWindow)

-- | The ID of the event window.
instanceEventWindow_instanceEventWindowId :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_instanceEventWindowId = Lens.lens (\InstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@InstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: InstanceEventWindow)

-- | The name of the event window.
instanceEventWindow_name :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_name = Lens.lens (\InstanceEventWindow' {name} -> name) (\s@InstanceEventWindow' {} a -> s {name = a} :: InstanceEventWindow)

-- | The cron expression defined for the event window.
instanceEventWindow_cronExpression :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_cronExpression = Lens.lens (\InstanceEventWindow' {cronExpression} -> cronExpression) (\s@InstanceEventWindow' {} a -> s {cronExpression = a} :: InstanceEventWindow)

-- | The instance tags associated with the event window.
instanceEventWindow_tags :: Lens.Lens' InstanceEventWindow (Prelude.Maybe [Tag])
instanceEventWindow_tags = Lens.lens (\InstanceEventWindow' {tags} -> tags) (\s@InstanceEventWindow' {} a -> s {tags = a} :: InstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

-- | One or more time ranges defined for the event window.
instanceEventWindow_timeRanges :: Lens.Lens' InstanceEventWindow (Prelude.Maybe [InstanceEventWindowTimeRange])
instanceEventWindow_timeRanges = Lens.lens (\InstanceEventWindow' {timeRanges} -> timeRanges) (\s@InstanceEventWindow' {} a -> s {timeRanges = a} :: InstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML InstanceEventWindow where
  parseXML x =
    InstanceEventWindow'
      Prelude.<$> (x Core..@? "state")
      Prelude.<*> (x Core..@? "associationTarget")
      Prelude.<*> (x Core..@? "instanceEventWindowId")
      Prelude.<*> (x Core..@? "name")
      Prelude.<*> (x Core..@? "cronExpression")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "timeRangeSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable InstanceEventWindow where
  hashWithSalt salt' InstanceEventWindow' {..} =
    salt' `Prelude.hashWithSalt` timeRanges
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceEventWindowId
      `Prelude.hashWithSalt` associationTarget
      `Prelude.hashWithSalt` state

instance Prelude.NFData InstanceEventWindow where
  rnf InstanceEventWindow' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf timeRanges
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf cronExpression
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceEventWindowId
      `Prelude.seq` Prelude.rnf associationTarget
