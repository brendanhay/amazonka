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
-- Module      : Amazonka.S3.Types.Part
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Part where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Container for elements related to a part.
--
-- /See:/ 'newPart' smart constructor.
data Part = Part'
  { -- | Entity tag returned when the part was uploaded.
    eTag :: Prelude.Maybe ETag,
    -- | Size in bytes of the uploaded part data.
    size :: Prelude.Maybe Prelude.Integer,
    -- | Part number identifying the part. This is a positive integer between 1
    -- and 10,000.
    partNumber :: Prelude.Maybe Prelude.Int,
    -- | Date and time at which the part was uploaded.
    lastModified :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'size', 'part_size' - Size in bytes of the uploaded part data.
--
-- 'partNumber', 'part_partNumber' - Part number identifying the part. This is a positive integer between 1
-- and 10,000.
--
-- 'lastModified', 'part_lastModified' - Date and time at which the part was uploaded.
newPart ::
  Part
newPart =
  Part'
    { eTag = Prelude.Nothing,
      size = Prelude.Nothing,
      partNumber = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | Entity tag returned when the part was uploaded.
part_eTag :: Lens.Lens' Part (Prelude.Maybe ETag)
part_eTag = Lens.lens (\Part' {eTag} -> eTag) (\s@Part' {} a -> s {eTag = a} :: Part)

-- | Size in bytes of the uploaded part data.
part_size :: Lens.Lens' Part (Prelude.Maybe Prelude.Integer)
part_size = Lens.lens (\Part' {size} -> size) (\s@Part' {} a -> s {size = a} :: Part)

-- | Part number identifying the part. This is a positive integer between 1
-- and 10,000.
part_partNumber :: Lens.Lens' Part (Prelude.Maybe Prelude.Int)
part_partNumber = Lens.lens (\Part' {partNumber} -> partNumber) (\s@Part' {} a -> s {partNumber = a} :: Part)

-- | Date and time at which the part was uploaded.
part_lastModified :: Lens.Lens' Part (Prelude.Maybe Prelude.UTCTime)
part_lastModified = Lens.lens (\Part' {lastModified} -> lastModified) (\s@Part' {} a -> s {lastModified = a} :: Part) Prelude.. Lens.mapping Core._Time

instance Core.FromXML Part where
  parseXML x =
    Part'
      Prelude.<$> (x Core..@? "ETag")
      Prelude.<*> (x Core..@? "Size")
      Prelude.<*> (x Core..@? "PartNumber")
      Prelude.<*> (x Core..@? "LastModified")

instance Prelude.Hashable Part where
  hashWithSalt _salt Part' {..} =
    _salt `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` partNumber
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData Part where
  rnf Part' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf partNumber
      `Prelude.seq` Prelude.rnf lastModified
