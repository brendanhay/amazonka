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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.S3BucketSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.S3BucketSinkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for the S3 bucket.
--
-- /See:/ 'newS3BucketSinkConfiguration' smart constructor.
data S3BucketSinkConfiguration = S3BucketSinkConfiguration'
  { -- | The destination URL of the S3 bucket.
    destination :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 's3BucketSinkConfiguration_destination' - The destination URL of the S3 bucket.
newS3BucketSinkConfiguration ::
  -- | 'destination'
  Prelude.Text ->
  S3BucketSinkConfiguration
newS3BucketSinkConfiguration pDestination_ =
  S3BucketSinkConfiguration'
    { destination =
        Data._Sensitive Lens.# pDestination_
    }

-- | The destination URL of the S3 bucket.
s3BucketSinkConfiguration_destination :: Lens.Lens' S3BucketSinkConfiguration Prelude.Text
s3BucketSinkConfiguration_destination = Lens.lens (\S3BucketSinkConfiguration' {destination} -> destination) (\s@S3BucketSinkConfiguration' {} a -> s {destination = a} :: S3BucketSinkConfiguration) Prelude.. Data._Sensitive

instance Data.FromJSON S3BucketSinkConfiguration where
  parseJSON =
    Data.withObject
      "S3BucketSinkConfiguration"
      ( \x ->
          S3BucketSinkConfiguration'
            Prelude.<$> (x Data..: "Destination")
      )

instance Prelude.Hashable S3BucketSinkConfiguration where
  hashWithSalt _salt S3BucketSinkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` destination

instance Prelude.NFData S3BucketSinkConfiguration where
  rnf S3BucketSinkConfiguration' {..} =
    Prelude.rnf destination

instance Data.ToJSON S3BucketSinkConfiguration where
  toJSON S3BucketSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Destination" Data..= destination)]
      )
