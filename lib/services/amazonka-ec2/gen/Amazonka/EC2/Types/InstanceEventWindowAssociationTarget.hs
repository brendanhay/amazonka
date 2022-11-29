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
-- Module      : Amazonka.EC2.Types.InstanceEventWindowAssociationTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceEventWindowAssociationTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | One or more targets associated with the event window.
--
-- /See:/ 'newInstanceEventWindowAssociationTarget' smart constructor.
data InstanceEventWindowAssociationTarget = InstanceEventWindowAssociationTarget'
  { -- | The instance tags associated with the event window. Any instances
    -- associated with the tags will be associated with the event window.
    tags :: Prelude.Maybe [Tag],
    -- | The IDs of the Dedicated Hosts associated with the event window.
    dedicatedHostIds :: Prelude.Maybe [Prelude.Text],
    -- | The IDs of the instances associated with the event window.
    instanceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceEventWindowAssociationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'instanceEventWindowAssociationTarget_tags' - The instance tags associated with the event window. Any instances
-- associated with the tags will be associated with the event window.
--
-- 'dedicatedHostIds', 'instanceEventWindowAssociationTarget_dedicatedHostIds' - The IDs of the Dedicated Hosts associated with the event window.
--
-- 'instanceIds', 'instanceEventWindowAssociationTarget_instanceIds' - The IDs of the instances associated with the event window.
newInstanceEventWindowAssociationTarget ::
  InstanceEventWindowAssociationTarget
newInstanceEventWindowAssociationTarget =
  InstanceEventWindowAssociationTarget'
    { tags =
        Prelude.Nothing,
      dedicatedHostIds = Prelude.Nothing,
      instanceIds = Prelude.Nothing
    }

-- | The instance tags associated with the event window. Any instances
-- associated with the tags will be associated with the event window.
instanceEventWindowAssociationTarget_tags :: Lens.Lens' InstanceEventWindowAssociationTarget (Prelude.Maybe [Tag])
instanceEventWindowAssociationTarget_tags = Lens.lens (\InstanceEventWindowAssociationTarget' {tags} -> tags) (\s@InstanceEventWindowAssociationTarget' {} a -> s {tags = a} :: InstanceEventWindowAssociationTarget) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the Dedicated Hosts associated with the event window.
instanceEventWindowAssociationTarget_dedicatedHostIds :: Lens.Lens' InstanceEventWindowAssociationTarget (Prelude.Maybe [Prelude.Text])
instanceEventWindowAssociationTarget_dedicatedHostIds = Lens.lens (\InstanceEventWindowAssociationTarget' {dedicatedHostIds} -> dedicatedHostIds) (\s@InstanceEventWindowAssociationTarget' {} a -> s {dedicatedHostIds = a} :: InstanceEventWindowAssociationTarget) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the instances associated with the event window.
instanceEventWindowAssociationTarget_instanceIds :: Lens.Lens' InstanceEventWindowAssociationTarget (Prelude.Maybe [Prelude.Text])
instanceEventWindowAssociationTarget_instanceIds = Lens.lens (\InstanceEventWindowAssociationTarget' {instanceIds} -> instanceIds) (\s@InstanceEventWindowAssociationTarget' {} a -> s {instanceIds = a} :: InstanceEventWindowAssociationTarget) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromXML
    InstanceEventWindowAssociationTarget
  where
  parseXML x =
    InstanceEventWindowAssociationTarget'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "dedicatedHostIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "instanceIdSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    InstanceEventWindowAssociationTarget
  where
  hashWithSalt
    _salt
    InstanceEventWindowAssociationTarget' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` dedicatedHostIds
        `Prelude.hashWithSalt` instanceIds

instance
  Prelude.NFData
    InstanceEventWindowAssociationTarget
  where
  rnf InstanceEventWindowAssociationTarget' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dedicatedHostIds
      `Prelude.seq` Prelude.rnf instanceIds
