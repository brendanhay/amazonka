{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.ThingGroupMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupMetadata where

import Network.AWS.IoT.Types.GroupNameAndArn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Thing group metadata.
--
-- /See:/ 'newThingGroupMetadata' smart constructor.
data ThingGroupMetadata = ThingGroupMetadata'
  { -- | The parent thing group name.
    parentGroupName :: Prelude.Maybe Prelude.Text,
    -- | The UNIX timestamp of when the thing group was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The root parent thing group.
    rootToParentThingGroups :: Prelude.Maybe [GroupNameAndArn]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ThingGroupMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentGroupName', 'thingGroupMetadata_parentGroupName' - The parent thing group name.
--
-- 'creationDate', 'thingGroupMetadata_creationDate' - The UNIX timestamp of when the thing group was created.
--
-- 'rootToParentThingGroups', 'thingGroupMetadata_rootToParentThingGroups' - The root parent thing group.
newThingGroupMetadata ::
  ThingGroupMetadata
newThingGroupMetadata =
  ThingGroupMetadata'
    { parentGroupName =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      rootToParentThingGroups = Prelude.Nothing
    }

-- | The parent thing group name.
thingGroupMetadata_parentGroupName :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.Text)
thingGroupMetadata_parentGroupName = Lens.lens (\ThingGroupMetadata' {parentGroupName} -> parentGroupName) (\s@ThingGroupMetadata' {} a -> s {parentGroupName = a} :: ThingGroupMetadata)

-- | The UNIX timestamp of when the thing group was created.
thingGroupMetadata_creationDate :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe Prelude.UTCTime)
thingGroupMetadata_creationDate = Lens.lens (\ThingGroupMetadata' {creationDate} -> creationDate) (\s@ThingGroupMetadata' {} a -> s {creationDate = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The root parent thing group.
thingGroupMetadata_rootToParentThingGroups :: Lens.Lens' ThingGroupMetadata (Prelude.Maybe [GroupNameAndArn])
thingGroupMetadata_rootToParentThingGroups = Lens.lens (\ThingGroupMetadata' {rootToParentThingGroups} -> rootToParentThingGroups) (\s@ThingGroupMetadata' {} a -> s {rootToParentThingGroups = a} :: ThingGroupMetadata) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ThingGroupMetadata where
  parseJSON =
    Prelude.withObject
      "ThingGroupMetadata"
      ( \x ->
          ThingGroupMetadata'
            Prelude.<$> (x Prelude..:? "parentGroupName")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> ( x Prelude..:? "rootToParentThingGroups"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ThingGroupMetadata

instance Prelude.NFData ThingGroupMetadata
