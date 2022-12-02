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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.GroupNameAndArn
import qualified Amazonka.Prelude as Prelude

-- | Thing group metadata.
--
-- /See:/ 'newThingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { -- | The root parent thing group.
    rootToParentThingGroups :: Prelude.Maybe [GroupNameAndArn],
    -- | The UNIX timestamp of when the thing group was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The parent thing group name.
    parentGroupName :: Prelude.Maybe Prelude.Text
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
-- 'creationDate', 'thingGroupMetadata_creationDate' - The UNIX timestamp of when the thing group was created.
--
-- 'parentGroupName', 'thingGroupMetadata_parentGroupName' - The parent thing group name.
newThingGroupMetadata ::
  ThingGroupMetadata
newThingGroupMetadata =
  ThingGroupMetadata'
    { rootToParentThingGroups =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      parentGroupName = Prelude.Nothing
    }

-- | The root parent thing group.
thingGroupMetadata_rootToParentThingGroups :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe [GroupNameAndArn])
thingGroupMetadata_rootToParentThingGroups = Lens.lens (\ThingGroupMetadata' {rootToParentThingGroups} -> rootToParentThingGroups) (\s@ThingGroupMetadata' {} a -> s {rootToParentThingGroups = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The UNIX timestamp of when the thing group was created.
thingGroupMetadata_creationDate :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.UTCTime)
thingGroupMetadata_creationDate = Lens.lens (\ThingGroupMetadata' {creationDate} -> creationDate) (\s@ThingGroupMetadata' {} a -> s {creationDate = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Data._Time

-- | The parent thing group name.
thingGroupMetadata_parentGroupName :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.Text)
thingGroupMetadata_parentGroupName = Lens.lens (\ThingGroupMetadata' {parentGroupName} -> parentGroupName) (\s@ThingGroupMetadata' {} a -> s {parentGroupName = a} :: ThingGroupMetadata)

instance Data.FromJSON ThingGroupMetadata where
  parseJSON =
    Data.withObject
      "ThingGroupMetadata"
      ( \x ->
          ThingGroupMetadata'
            Prelude.<$> ( x Data..:? "rootToParentThingGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "parentGroupName")
      )

instance Prelude.Hashable ThingGroupMetadata where
  hashWithSalt _salt ThingGroupMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` rootToParentThingGroups
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` parentGroupName

instance Prelude.NFData ThingGroupMetadata where
  rnf ThingGroupMetadata' {..} =
    Prelude.rnf rootToParentThingGroups
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf parentGroupName
