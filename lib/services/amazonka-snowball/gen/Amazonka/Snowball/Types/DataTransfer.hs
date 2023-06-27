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
-- Module      : Amazonka.Snowball.Types.DataTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.DataTransfer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the real-time status of a Snow device\'s data transfer while the
-- device is at Amazon Web Services. This data is only available while a
-- job has a @JobState@ value of @InProgress@, for both import and export
-- jobs.
--
-- /See:/ 'newDataTransfer' smart constructor.
data DataTransfer = DataTransfer'
  { -- | The number of bytes transferred between a Snow device and Amazon S3.
    bytesTransferred :: Prelude.Maybe Prelude.Integer,
    -- | The number of objects transferred between a Snow device and Amazon S3.
    objectsTransferred :: Prelude.Maybe Prelude.Integer,
    -- | The total bytes of data for a transfer between a Snow device and Amazon
    -- S3. This value is set to 0 (zero) until all the keys that will be
    -- transferred have been listed.
    totalBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects for a transfer between a Snow device and
    -- Amazon S3. This value is set to 0 (zero) until all the keys that will be
    -- transferred have been listed.
    totalObjects :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesTransferred', 'dataTransfer_bytesTransferred' - The number of bytes transferred between a Snow device and Amazon S3.
--
-- 'objectsTransferred', 'dataTransfer_objectsTransferred' - The number of objects transferred between a Snow device and Amazon S3.
--
-- 'totalBytes', 'dataTransfer_totalBytes' - The total bytes of data for a transfer between a Snow device and Amazon
-- S3. This value is set to 0 (zero) until all the keys that will be
-- transferred have been listed.
--
-- 'totalObjects', 'dataTransfer_totalObjects' - The total number of objects for a transfer between a Snow device and
-- Amazon S3. This value is set to 0 (zero) until all the keys that will be
-- transferred have been listed.
newDataTransfer ::
  DataTransfer
newDataTransfer =
  DataTransfer'
    { bytesTransferred = Prelude.Nothing,
      objectsTransferred = Prelude.Nothing,
      totalBytes = Prelude.Nothing,
      totalObjects = Prelude.Nothing
    }

-- | The number of bytes transferred between a Snow device and Amazon S3.
dataTransfer_bytesTransferred :: Lens.Lens' DataTransfer (Prelude.Maybe Prelude.Integer)
dataTransfer_bytesTransferred = Lens.lens (\DataTransfer' {bytesTransferred} -> bytesTransferred) (\s@DataTransfer' {} a -> s {bytesTransferred = a} :: DataTransfer)

-- | The number of objects transferred between a Snow device and Amazon S3.
dataTransfer_objectsTransferred :: Lens.Lens' DataTransfer (Prelude.Maybe Prelude.Integer)
dataTransfer_objectsTransferred = Lens.lens (\DataTransfer' {objectsTransferred} -> objectsTransferred) (\s@DataTransfer' {} a -> s {objectsTransferred = a} :: DataTransfer)

-- | The total bytes of data for a transfer between a Snow device and Amazon
-- S3. This value is set to 0 (zero) until all the keys that will be
-- transferred have been listed.
dataTransfer_totalBytes :: Lens.Lens' DataTransfer (Prelude.Maybe Prelude.Integer)
dataTransfer_totalBytes = Lens.lens (\DataTransfer' {totalBytes} -> totalBytes) (\s@DataTransfer' {} a -> s {totalBytes = a} :: DataTransfer)

-- | The total number of objects for a transfer between a Snow device and
-- Amazon S3. This value is set to 0 (zero) until all the keys that will be
-- transferred have been listed.
dataTransfer_totalObjects :: Lens.Lens' DataTransfer (Prelude.Maybe Prelude.Integer)
dataTransfer_totalObjects = Lens.lens (\DataTransfer' {totalObjects} -> totalObjects) (\s@DataTransfer' {} a -> s {totalObjects = a} :: DataTransfer)

instance Data.FromJSON DataTransfer where
  parseJSON =
    Data.withObject
      "DataTransfer"
      ( \x ->
          DataTransfer'
            Prelude.<$> (x Data..:? "BytesTransferred")
            Prelude.<*> (x Data..:? "ObjectsTransferred")
            Prelude.<*> (x Data..:? "TotalBytes")
            Prelude.<*> (x Data..:? "TotalObjects")
      )

instance Prelude.Hashable DataTransfer where
  hashWithSalt _salt DataTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` bytesTransferred
      `Prelude.hashWithSalt` objectsTransferred
      `Prelude.hashWithSalt` totalBytes
      `Prelude.hashWithSalt` totalObjects

instance Prelude.NFData DataTransfer where
  rnf DataTransfer' {..} =
    Prelude.rnf bytesTransferred
      `Prelude.seq` Prelude.rnf objectsTransferred
      `Prelude.seq` Prelude.rnf totalBytes
      `Prelude.seq` Prelude.rnf totalObjects
