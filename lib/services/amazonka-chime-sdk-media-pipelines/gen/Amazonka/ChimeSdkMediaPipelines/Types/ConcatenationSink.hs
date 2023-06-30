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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSink where

import Amazonka.ChimeSdkMediaPipelines.Types.ConcatenationSinkType
import Amazonka.ChimeSdkMediaPipelines.Types.S3BucketSinkConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data sink of the configuration object.
--
-- /See:/ 'newConcatenationSink' smart constructor.
data ConcatenationSink = ConcatenationSink'
  { -- | The type of data sink in the configuration object.
    type' :: ConcatenationSinkType,
    -- | The configuration settings for an Amazon S3 bucket sink.
    s3BucketSinkConfiguration :: S3BucketSinkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConcatenationSink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'concatenationSink_type' - The type of data sink in the configuration object.
--
-- 's3BucketSinkConfiguration', 'concatenationSink_s3BucketSinkConfiguration' - The configuration settings for an Amazon S3 bucket sink.
newConcatenationSink ::
  -- | 'type''
  ConcatenationSinkType ->
  -- | 's3BucketSinkConfiguration'
  S3BucketSinkConfiguration ->
  ConcatenationSink
newConcatenationSink
  pType_
  pS3BucketSinkConfiguration_ =
    ConcatenationSink'
      { type' = pType_,
        s3BucketSinkConfiguration =
          pS3BucketSinkConfiguration_
      }

-- | The type of data sink in the configuration object.
concatenationSink_type :: Lens.Lens' ConcatenationSink ConcatenationSinkType
concatenationSink_type = Lens.lens (\ConcatenationSink' {type'} -> type') (\s@ConcatenationSink' {} a -> s {type' = a} :: ConcatenationSink)

-- | The configuration settings for an Amazon S3 bucket sink.
concatenationSink_s3BucketSinkConfiguration :: Lens.Lens' ConcatenationSink S3BucketSinkConfiguration
concatenationSink_s3BucketSinkConfiguration = Lens.lens (\ConcatenationSink' {s3BucketSinkConfiguration} -> s3BucketSinkConfiguration) (\s@ConcatenationSink' {} a -> s {s3BucketSinkConfiguration = a} :: ConcatenationSink)

instance Data.FromJSON ConcatenationSink where
  parseJSON =
    Data.withObject
      "ConcatenationSink"
      ( \x ->
          ConcatenationSink'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "S3BucketSinkConfiguration")
      )

instance Prelude.Hashable ConcatenationSink where
  hashWithSalt _salt ConcatenationSink' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` s3BucketSinkConfiguration

instance Prelude.NFData ConcatenationSink where
  rnf ConcatenationSink' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf s3BucketSinkConfiguration

instance Data.ToJSON ConcatenationSink where
  toJSON ConcatenationSink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ( "S3BucketSinkConfiguration"
                  Data..= s3BucketSinkConfiguration
              )
          ]
      )
