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
-- Module      : Amazonka.Snowball.Types.S3Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.S3Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.KeyRange
import Amazonka.Snowball.Types.TargetOnDeviceService

-- | Each @S3Resource@ object represents an Amazon S3 bucket that your
-- transferred data will be exported from or imported into. For export
-- jobs, this object can have an optional @KeyRange@ value. The length of
-- the range is defined at job creation, and has either an inclusive
-- @BeginMarker@, an inclusive @EndMarker@, or both. Ranges are UTF-8
-- binary sorted.
--
-- /See:/ 'newS3Resource' smart constructor.
data S3Resource = S3Resource'
  { -- | Specifies the service or services on the Snow Family device that your
    -- transferred data will be exported from or imported into. Amazon Web
    -- Services Snow Family supports Amazon S3 and NFS (Network File System).
    targetOnDeviceServices :: Prelude.Maybe [TargetOnDeviceService],
    -- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
    bucketArn :: Prelude.Maybe Prelude.Text,
    -- | For export jobs, you can provide an optional @KeyRange@ within a
    -- specific Amazon S3 bucket. The length of the range is defined at job
    -- creation, and has either an inclusive @BeginMarker@, an inclusive
    -- @EndMarker@, or both. Ranges are UTF-8 binary sorted.
    keyRange :: Prelude.Maybe KeyRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetOnDeviceServices', 's3Resource_targetOnDeviceServices' - Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family supports Amazon S3 and NFS (Network File System).
--
-- 'bucketArn', 's3Resource_bucketArn' - The Amazon Resource Name (ARN) of an Amazon S3 bucket.
--
-- 'keyRange', 's3Resource_keyRange' - For export jobs, you can provide an optional @KeyRange@ within a
-- specific Amazon S3 bucket. The length of the range is defined at job
-- creation, and has either an inclusive @BeginMarker@, an inclusive
-- @EndMarker@, or both. Ranges are UTF-8 binary sorted.
newS3Resource ::
  S3Resource
newS3Resource =
  S3Resource'
    { targetOnDeviceServices =
        Prelude.Nothing,
      bucketArn = Prelude.Nothing,
      keyRange = Prelude.Nothing
    }

-- | Specifies the service or services on the Snow Family device that your
-- transferred data will be exported from or imported into. Amazon Web
-- Services Snow Family supports Amazon S3 and NFS (Network File System).
s3Resource_targetOnDeviceServices :: Lens.Lens' S3Resource (Prelude.Maybe [TargetOnDeviceService])
s3Resource_targetOnDeviceServices = Lens.lens (\S3Resource' {targetOnDeviceServices} -> targetOnDeviceServices) (\s@S3Resource' {} a -> s {targetOnDeviceServices = a} :: S3Resource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an Amazon S3 bucket.
s3Resource_bucketArn :: Lens.Lens' S3Resource (Prelude.Maybe Prelude.Text)
s3Resource_bucketArn = Lens.lens (\S3Resource' {bucketArn} -> bucketArn) (\s@S3Resource' {} a -> s {bucketArn = a} :: S3Resource)

-- | For export jobs, you can provide an optional @KeyRange@ within a
-- specific Amazon S3 bucket. The length of the range is defined at job
-- creation, and has either an inclusive @BeginMarker@, an inclusive
-- @EndMarker@, or both. Ranges are UTF-8 binary sorted.
s3Resource_keyRange :: Lens.Lens' S3Resource (Prelude.Maybe KeyRange)
s3Resource_keyRange = Lens.lens (\S3Resource' {keyRange} -> keyRange) (\s@S3Resource' {} a -> s {keyRange = a} :: S3Resource)

instance Core.FromJSON S3Resource where
  parseJSON =
    Core.withObject
      "S3Resource"
      ( \x ->
          S3Resource'
            Prelude.<$> ( x Core..:? "TargetOnDeviceServices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BucketArn")
            Prelude.<*> (x Core..:? "KeyRange")
      )

instance Prelude.Hashable S3Resource where
  hashWithSalt _salt S3Resource' {..} =
    _salt `Prelude.hashWithSalt` targetOnDeviceServices
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` keyRange

instance Prelude.NFData S3Resource where
  rnf S3Resource' {..} =
    Prelude.rnf targetOnDeviceServices
      `Prelude.seq` Prelude.rnf bucketArn
      `Prelude.seq` Prelude.rnf keyRange

instance Core.ToJSON S3Resource where
  toJSON S3Resource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetOnDeviceServices" Core..=)
              Prelude.<$> targetOnDeviceServices,
            ("BucketArn" Core..=) Prelude.<$> bucketArn,
            ("KeyRange" Core..=) Prelude.<$> keyRange
          ]
      )
