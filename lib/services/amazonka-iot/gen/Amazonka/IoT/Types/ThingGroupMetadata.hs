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
-- Module      : Amazonka.IoT.Types.ThingGroupMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupMetadata where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.GroupNameAndArn
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Thing group metadata.
--
-- /See:/ 'newThingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { -- | The root parent thing group.
    rootToParentThingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The parent thing group name.
    parentGroupName :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the thing group was created.
    creationDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootToParentThingGroups', 'thingGroupMetadata_rootToParentThingGroups' - The root parent thing group.
--
-- 'parentGroupName', 'thingGroupMetadata_parentGroupName' - The parent thing group name.
--
-- 'creationDate', 'thingGroupMetadata_creationDate' - The UNIX timestamp of when the thing group was created.
newThingGroupMetadata ::
  ThingGroupMetadata
newThingGroupMetadata =
  ThingGroupMetadata'
    { rootToParentThingGroups =
        Prelude.Nothing,
      parentGroupName = Prelude.Nothing,
      creationDate = Prelude.Nothing
    }

-- | The root parent thing group.
thingGroupMetadata_rootToParentThingGroups :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe [GroupNameAndArn])
thingGroupMetadata_rootToParentThingGroups = Lens.lens (\ThingGroupMetadata' {rootToParentThingGroups} -> rootToParentThingGroups) (\s@ThingGroupMetadata' {} a -> s {rootToParentThingGroups = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The parent thing group name.
thingGroupMetadata_parentGroupName :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.Text)
thingGroupMetadata_parentGroupName = Lens.lens (\ThingGroupMetadata' {parentGroupName} -> parentGroupName) (\s@ThingGroupMetadata' {} a -> s {parentGroupName = a} :: ThingGroupMetadata)

-- | The UNIX timestamp of when the thing group was created.
thingGroupMetadata_creationDate :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.UTCTime)
thingGroupMetadata_creationDate = Lens.lens (\ThingGroupMetadata' {creationDate} -> creationDate) (\s@ThingGroupMetadata' {} a -> s {creationDate = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ThingGroupMetadata where
  parseJSON =
    Core.withObject
      "ThingGroupMetadata"
      ( \x ->
          ThingGroupMetadata'
            Prelude.<$> ( x Core..:? "rootToParentThingGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "parentGroupName")
            Prelude.<*> (x Core..:? "creationDate")
      )

instance Prelude.Hashable ThingGroupMetadata where
  hashWithSalt _salt ThingGroupMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` rootToParentThingGroups
      `Prelude.hashWithSalt` parentGroupName
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData ThingGroupMetadata where
  rnf ThingGroupMetadata' {..} =
    Prelude.rnf rootToParentThingGroups
      `Prelude.seq` Prelude.rnf parentGroupName
      `Prelude.seq` Prelude.rnf creationDate
