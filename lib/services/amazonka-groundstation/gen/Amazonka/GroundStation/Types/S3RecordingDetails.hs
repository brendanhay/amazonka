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
-- Module      : Amazonka.GroundStation.Types.S3RecordingDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.S3RecordingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about an S3 recording @Config@ used in a contact.
--
-- /See:/ 'newS3RecordingDetails' smart constructor.
data S3RecordingDetails = S3RecordingDetails'
  { -- | Key template used for the S3 Recording Configuration
    keyTemplate :: Prelude.Maybe Prelude.Text,
    -- | ARN of the bucket used.
    bucketArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3RecordingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyTemplate', 's3RecordingDetails_keyTemplate' - Key template used for the S3 Recording Configuration
--
-- 'bucketArn', 's3RecordingDetails_bucketArn' - ARN of the bucket used.
newS3RecordingDetails ::
  S3RecordingDetails
newS3RecordingDetails =
  S3RecordingDetails'
    { keyTemplate = Prelude.Nothing,
      bucketArn = Prelude.Nothing
    }

-- | Key template used for the S3 Recording Configuration
s3RecordingDetails_keyTemplate :: Lens.Lens' S3RecordingDetails (Prelude.Maybe Prelude.Text)
s3RecordingDetails_keyTemplate = Lens.lens (\S3RecordingDetails' {keyTemplate} -> keyTemplate) (\s@S3RecordingDetails' {} a -> s {keyTemplate = a} :: S3RecordingDetails)

-- | ARN of the bucket used.
s3RecordingDetails_bucketArn :: Lens.Lens' S3RecordingDetails (Prelude.Maybe Prelude.Text)
s3RecordingDetails_bucketArn = Lens.lens (\S3RecordingDetails' {bucketArn} -> bucketArn) (\s@S3RecordingDetails' {} a -> s {bucketArn = a} :: S3RecordingDetails)

instance Core.FromJSON S3RecordingDetails where
  parseJSON =
    Core.withObject
      "S3RecordingDetails"
      ( \x ->
          S3RecordingDetails'
            Prelude.<$> (x Core..:? "keyTemplate")
            Prelude.<*> (x Core..:? "bucketArn")
      )

instance Prelude.Hashable S3RecordingDetails where
  hashWithSalt _salt S3RecordingDetails' {..} =
    _salt `Prelude.hashWithSalt` keyTemplate
      `Prelude.hashWithSalt` bucketArn

instance Prelude.NFData S3RecordingDetails where
  rnf S3RecordingDetails' {..} =
    Prelude.rnf keyTemplate
      `Prelude.seq` Prelude.rnf bucketArn
