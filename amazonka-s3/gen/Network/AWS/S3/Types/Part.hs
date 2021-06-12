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
-- Module      : Network.AWS.S3.Types.Part
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Part where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | Container for elements related to a part.
--
-- /See:/ 'newPart' smart constructor.
data Part = Part'
  { -- | Entity tag returned when the part was uploaded.
    eTag :: Core.Maybe ETag,
    -- | Part number identifying the part. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Core.Maybe Core.Int,
    -- | Date and time at which the part was uploaded.
    lastModified :: Core.Maybe Core.ISO8601,
    -- | Size in bytes of the uploaded part data.
    size :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Part' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'part_eTag' - Entity tag returned when the part was uploaded.
--
-- 'partNumber', 'part_partNumber' - Part number identifying the part. This is a positive integer between 1
-- and 10,000.
--
-- 'lastModified', 'part_lastModified' - Date and time at which the part was uploaded.
--
-- 'size', 'part_size' - Size in bytes of the uploaded part data.
newPart ::
  Part
newPart =
  Part'
    { eTag = Core.Nothing,
      partNumber = Core.Nothing,
      lastModified = Core.Nothing,
      size = Core.Nothing
    }

-- | Entity tag returned when the part was uploaded.
part_eTag :: Lens.Lens' Part (Core.Maybe ETag)
part_eTag = Lens.lens (\Part' {eTag} -> eTag) (\s@Part' {} a -> s {eTag = a} :: Part)

-- | Part number identifying the part. This is a positive integer between 1
-- and 10,000.
part_partNumber :: Lens.Lens' Part (Core.Maybe Core.Int)
part_partNumber = Lens.lens (\Part' {partNumber} -> partNumber) (\s@Part' {} a -> s {partNumber = a} :: Part)

-- | Date and time at which the part was uploaded.
part_lastModified :: Lens.Lens' Part (Core.Maybe Core.UTCTime)
part_lastModified = Lens.lens (\Part' {lastModified} -> lastModified) (\s@Part' {} a -> s {lastModified = a} :: Part) Core.. Lens.mapping Core._Time

-- | Size in bytes of the uploaded part data.
part_size :: Lens.Lens' Part (Core.Maybe Core.Int)
part_size = Lens.lens (\Part' {size} -> size) (\s@Part' {} a -> s {size = a} :: Part)

instance Core.FromXML Part where
  parseXML x =
    Part'
      Core.<$> (x Core..@? "ETag")
      Core.<*> (x Core..@? "PartNumber")
      Core.<*> (x Core..@? "LastModified")
      Core.<*> (x Core..@? "Size")

instance Core.Hashable Part

instance Core.NFData Part
