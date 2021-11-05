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
-- Module      : Network.AWS.Transfer.Types.HomeDirectoryMapEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transfer.Types.HomeDirectoryMapEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an object that contains entries and targets for
-- @HomeDirectoryMappings@.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry:\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- If the target of a logical directory entry does not exist in Amazon S3
-- or EFS, the entry is ignored. As a workaround, you can use the Amazon S3
-- API or EFS API to create 0 byte objects as place holders for your
-- directory. If using the CLI, use the @s3api@ or @efsapi@ call instead of
-- @s3@ or @efs@ so you can use the put-object operation. For example, you
-- use the following:
-- @aws s3api put-object --bucket bucketname --key path\/to\/folder\/@.
-- Make sure that the end of the key name ends in a @\/@ for it to be
-- considered a folder.
--
-- /See:/ 'newHomeDirectoryMapEntry' smart constructor.
data HomeDirectoryMapEntry = HomeDirectoryMapEntry'
  { -- | Represents an entry for @HomeDirectoryMappings@.
    entry :: Prelude.Text,
    -- | Represents the map target that is used in a @HomeDirectorymapEntry@.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HomeDirectoryMapEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entry', 'homeDirectoryMapEntry_entry' - Represents an entry for @HomeDirectoryMappings@.
--
-- 'target', 'homeDirectoryMapEntry_target' - Represents the map target that is used in a @HomeDirectorymapEntry@.
newHomeDirectoryMapEntry ::
  -- | 'entry'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  HomeDirectoryMapEntry
newHomeDirectoryMapEntry pEntry_ pTarget_ =
  HomeDirectoryMapEntry'
    { entry = pEntry_,
      target = pTarget_
    }

-- | Represents an entry for @HomeDirectoryMappings@.
homeDirectoryMapEntry_entry :: Lens.Lens' HomeDirectoryMapEntry Prelude.Text
homeDirectoryMapEntry_entry = Lens.lens (\HomeDirectoryMapEntry' {entry} -> entry) (\s@HomeDirectoryMapEntry' {} a -> s {entry = a} :: HomeDirectoryMapEntry)

-- | Represents the map target that is used in a @HomeDirectorymapEntry@.
homeDirectoryMapEntry_target :: Lens.Lens' HomeDirectoryMapEntry Prelude.Text
homeDirectoryMapEntry_target = Lens.lens (\HomeDirectoryMapEntry' {target} -> target) (\s@HomeDirectoryMapEntry' {} a -> s {target = a} :: HomeDirectoryMapEntry)

instance Core.FromJSON HomeDirectoryMapEntry where
  parseJSON =
    Core.withObject
      "HomeDirectoryMapEntry"
      ( \x ->
          HomeDirectoryMapEntry'
            Prelude.<$> (x Core..: "Entry") Prelude.<*> (x Core..: "Target")
      )

instance Prelude.Hashable HomeDirectoryMapEntry

instance Prelude.NFData HomeDirectoryMapEntry

instance Core.ToJSON HomeDirectoryMapEntry where
  toJSON HomeDirectoryMapEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Entry" Core..= entry),
            Prelude.Just ("Target" Core..= target)
          ]
      )
