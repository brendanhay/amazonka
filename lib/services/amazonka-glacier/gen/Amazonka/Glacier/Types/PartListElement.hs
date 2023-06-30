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
-- Module      : Amazonka.Glacier.Types.PartListElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.PartListElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of the part sizes of the multipart upload.
--
-- /See:/ 'newPartListElement' smart constructor.
data PartListElement = PartListElement'
  { -- | The byte range of a part, inclusive of the upper value of the range.
    rangeInBytes :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the
    -- part. This field is never @null@.
    sHA256TreeHash :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartListElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rangeInBytes', 'partListElement_rangeInBytes' - The byte range of a part, inclusive of the upper value of the range.
--
-- 'sHA256TreeHash', 'partListElement_sHA256TreeHash' - The SHA256 tree hash value that Amazon S3 Glacier calculated for the
-- part. This field is never @null@.
newPartListElement ::
  PartListElement
newPartListElement =
  PartListElement'
    { rangeInBytes = Prelude.Nothing,
      sHA256TreeHash = Prelude.Nothing
    }

-- | The byte range of a part, inclusive of the upper value of the range.
partListElement_rangeInBytes :: Lens.Lens' PartListElement (Prelude.Maybe Prelude.Text)
partListElement_rangeInBytes = Lens.lens (\PartListElement' {rangeInBytes} -> rangeInBytes) (\s@PartListElement' {} a -> s {rangeInBytes = a} :: PartListElement)

-- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the
-- part. This field is never @null@.
partListElement_sHA256TreeHash :: Lens.Lens' PartListElement (Prelude.Maybe Prelude.Text)
partListElement_sHA256TreeHash = Lens.lens (\PartListElement' {sHA256TreeHash} -> sHA256TreeHash) (\s@PartListElement' {} a -> s {sHA256TreeHash = a} :: PartListElement)

instance Data.FromJSON PartListElement where
  parseJSON =
    Data.withObject
      "PartListElement"
      ( \x ->
          PartListElement'
            Prelude.<$> (x Data..:? "RangeInBytes")
            Prelude.<*> (x Data..:? "SHA256TreeHash")
      )

instance Prelude.Hashable PartListElement where
  hashWithSalt _salt PartListElement' {..} =
    _salt
      `Prelude.hashWithSalt` rangeInBytes
      `Prelude.hashWithSalt` sHA256TreeHash

instance Prelude.NFData PartListElement where
  rnf PartListElement' {..} =
    Prelude.rnf rangeInBytes
      `Prelude.seq` Prelude.rnf sHA256TreeHash
