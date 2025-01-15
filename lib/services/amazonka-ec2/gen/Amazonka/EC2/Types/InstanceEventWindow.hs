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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceEventWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceEventWindowAssociationTarget
import Amazonka.EC2.Types.InstanceEventWindowState
import Amazonka.EC2.Types.InstanceEventWindowTimeRange
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The event window.
--
-- /See:/ 'newInstanceEventWindow' smart constructor.
data InstanceEventWindow = InstanceEventWindow'
  { -- | One or more targets associated with the event window.
    associationTarget :: Prelude.Maybe InstanceEventWindowAssociationTarget,
    -- | The cron expression defined for the event window.
    cronExpression :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event window.
    instanceEventWindowId :: Prelude.Maybe Prelude.Text,
    -- | The name of the event window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current state of the event window.
    state :: Prelude.Maybe InstanceEventWindowState,
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
-- 'associationTarget', 'instanceEventWindow_associationTarget' - One or more targets associated with the event window.
--
-- 'cronExpression', 'instanceEventWindow_cronExpression' - The cron expression defined for the event window.
--
-- 'instanceEventWindowId', 'instanceEventWindow_instanceEventWindowId' - The ID of the event window.
--
-- 'name', 'instanceEventWindow_name' - The name of the event window.
--
-- 'state', 'instanceEventWindow_state' - The current state of the event window.
--
-- 'tags', 'instanceEventWindow_tags' - The instance tags associated with the event window.
--
-- 'timeRanges', 'instanceEventWindow_timeRanges' - One or more time ranges defined for the event window.
newInstanceEventWindow ::
  InstanceEventWindow
newInstanceEventWindow =
  InstanceEventWindow'
    { associationTarget =
        Prelude.Nothing,
      cronExpression = Prelude.Nothing,
      instanceEventWindowId = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeRanges = Prelude.Nothing
    }

-- | One or more targets associated with the event window.
instanceEventWindow_associationTarget :: Lens.Lens' InstanceEventWindow (Prelude.Maybe InstanceEventWindowAssociationTarget)
instanceEventWindow_associationTarget = Lens.lens (\InstanceEventWindow' {associationTarget} -> associationTarget) (\s@InstanceEventWindow' {} a -> s {associationTarget = a} :: InstanceEventWindow)

-- | The cron expression defined for the event window.
instanceEventWindow_cronExpression :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_cronExpression = Lens.lens (\InstanceEventWindow' {cronExpression} -> cronExpression) (\s@InstanceEventWindow' {} a -> s {cronExpression = a} :: InstanceEventWindow)

-- | The ID of the event window.
instanceEventWindow_instanceEventWindowId :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_instanceEventWindowId = Lens.lens (\InstanceEventWindow' {instanceEventWindowId} -> instanceEventWindowId) (\s@InstanceEventWindow' {} a -> s {instanceEventWindowId = a} :: InstanceEventWindow)

-- | The name of the event window.
instanceEventWindow_name :: Lens.Lens' InstanceEventWindow (Prelude.Maybe Prelude.Text)
instanceEventWindow_name = Lens.lens (\InstanceEventWindow' {name} -> name) (\s@InstanceEventWindow' {} a -> s {name = a} :: InstanceEventWindow)

-- | The current state of the event window.
instanceEventWindow_state :: Lens.Lens' InstanceEventWindow (Prelude.Maybe InstanceEventWindowState)
instanceEventWindow_state = Lens.lens (\InstanceEventWindow' {state} -> state) (\s@InstanceEventWindow' {} a -> s {state = a} :: InstanceEventWindow)

-- | The instance tags associated with the event window.
instanceEventWindow_tags :: Lens.Lens' InstanceEventWindow (Prelude.Maybe [Tag])
instanceEventWindow_tags = Lens.lens (\InstanceEventWindow' {tags} -> tags) (\s@InstanceEventWindow' {} a -> s {tags = a} :: InstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

-- | One or more time ranges defined for the event window.
instanceEventWindow_timeRanges :: Lens.Lens' InstanceEventWindow (Prelude.Maybe [InstanceEventWindowTimeRange])
instanceEventWindow_timeRanges = Lens.lens (\InstanceEventWindow' {timeRanges} -> timeRanges) (\s@InstanceEventWindow' {} a -> s {timeRanges = a} :: InstanceEventWindow) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML InstanceEventWindow where
  parseXML x =
    InstanceEventWindow'
      Prelude.<$> (x Data..@? "associationTarget")
      Prelude.<*> (x Data..@? "cronExpression")
      Prelude.<*> (x Data..@? "instanceEventWindowId")
      Prelude.<*> (x Data..@? "name")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "timeRangeSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable InstanceEventWindow where
  hashWithSalt _salt InstanceEventWindow' {..} =
    _salt
      `Prelude.hashWithSalt` associationTarget
      `Prelude.hashWithSalt` cronExpression
      `Prelude.hashWithSalt` instanceEventWindowId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeRanges

instance Prelude.NFData InstanceEventWindow where
  rnf InstanceEventWindow' {..} =
    Prelude.rnf associationTarget `Prelude.seq`
      Prelude.rnf cronExpression `Prelude.seq`
        Prelude.rnf instanceEventWindowId `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf timeRanges
