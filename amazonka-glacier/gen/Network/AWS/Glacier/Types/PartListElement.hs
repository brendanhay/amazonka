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
-- Module      : Network.AWS.Glacier.Types.PartListElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.PartListElement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of the part sizes of the multipart upload.
--
-- /See:/ 'newPartListElement' smart constructor.
data PartListElement = PartListElement'
  { -- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the
    -- part. This field is never @null@.
    sHA256TreeHash :: Core.Maybe Core.Text,
    -- | The byte range of a part, inclusive of the upper value of the range.
    rangeInBytes :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sHA256TreeHash', 'partListElement_sHA256TreeHash' - The SHA256 tree hash value that Amazon S3 Glacier calculated for the
-- part. This field is never @null@.
--
-- 'rangeInBytes', 'partListElement_rangeInBytes' - The byte range of a part, inclusive of the upper value of the range.
newPartListElement ::
  PartListElement
newPartListElement =
  PartListElement'
    { sHA256TreeHash = Core.Nothing,
      rangeInBytes = Core.Nothing
    }

-- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the
-- part. This field is never @null@.
partListElement_sHA256TreeHash :: Lens.Lens' PartListElement (Core.Maybe Core.Text)
partListElement_sHA256TreeHash = Lens.lens (\PartListElement' {sHA256TreeHash} -> sHA256TreeHash) (\s@PartListElement' {} a -> s {sHA256TreeHash = a} :: PartListElement)

-- | The byte range of a part, inclusive of the upper value of the range.
partListElement_rangeInBytes :: Lens.Lens' PartListElement (Core.Maybe Core.Text)
partListElement_rangeInBytes = Lens.lens (\PartListElement' {rangeInBytes} -> rangeInBytes) (\s@PartListElement' {} a -> s {rangeInBytes = a} :: PartListElement)

instance Core.FromJSON PartListElement where
  parseJSON =
    Core.withObject
      "PartListElement"
      ( \x ->
          PartListElement'
            Core.<$> (x Core..:? "SHA256TreeHash")
            Core.<*> (x Core..:? "RangeInBytes")
      )

instance Core.Hashable PartListElement

instance Core.NFData PartListElement
