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
-- Module      : Network.AWS.S3.Types.MetadataEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetadataEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | A metadata key-value pair to store with an object.
--
-- /See:/ 'newMetadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { -- | Name of the Object.
    name :: Core.Maybe Core.Text,
    -- | Value of the Object.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MetadataEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'metadataEntry_name' - Name of the Object.
--
-- 'value', 'metadataEntry_value' - Value of the Object.
newMetadataEntry ::
  MetadataEntry
newMetadataEntry =
  MetadataEntry'
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | Name of the Object.
metadataEntry_name :: Lens.Lens' MetadataEntry (Core.Maybe Core.Text)
metadataEntry_name = Lens.lens (\MetadataEntry' {name} -> name) (\s@MetadataEntry' {} a -> s {name = a} :: MetadataEntry)

-- | Value of the Object.
metadataEntry_value :: Lens.Lens' MetadataEntry (Core.Maybe Core.Text)
metadataEntry_value = Lens.lens (\MetadataEntry' {value} -> value) (\s@MetadataEntry' {} a -> s {value = a} :: MetadataEntry)

instance Core.Hashable MetadataEntry

instance Core.NFData MetadataEntry

instance Core.ToXML MetadataEntry where
  toXML MetadataEntry' {..} =
    Core.mconcat
      ["Name" Core.@= name, "Value" Core.@= value]
