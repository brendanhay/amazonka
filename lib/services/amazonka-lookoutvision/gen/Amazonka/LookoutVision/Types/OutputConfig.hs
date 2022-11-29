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
-- Module      : Amazonka.LookoutVision.Types.OutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.OutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | The S3 location where Amazon Lookout for Vision saves model training
-- files.
--
-- /See:/ 'newOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The S3 location for the output.
    s3Location :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'outputConfig_s3Location' - The S3 location for the output.
newOutputConfig ::
  -- | 's3Location'
  S3Location ->
  OutputConfig
newOutputConfig pS3Location_ =
  OutputConfig' {s3Location = pS3Location_}

-- | The S3 location for the output.
outputConfig_s3Location :: Lens.Lens' OutputConfig S3Location
outputConfig_s3Location = Lens.lens (\OutputConfig' {s3Location} -> s3Location) (\s@OutputConfig' {} a -> s {s3Location = a} :: OutputConfig)

instance Core.FromJSON OutputConfig where
  parseJSON =
    Core.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig' Prelude.<$> (x Core..: "S3Location")
      )

instance Prelude.Hashable OutputConfig where
  hashWithSalt _salt OutputConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Location

instance Prelude.NFData OutputConfig where
  rnf OutputConfig' {..} = Prelude.rnf s3Location

instance Core.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Location" Core..= s3Location)]
      )
