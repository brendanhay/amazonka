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
-- Module      : Amazonka.HealthLake.Types.OutputDataConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.S3Configuration
import qualified Amazonka.Prelude as Prelude

-- | The output data configuration that was supplied when the export job was
-- created.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The output data configuration that was supplied when the export job was
    -- created.
    s3Configuration :: Prelude.Maybe S3Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Configuration', 'outputDataConfig_s3Configuration' - The output data configuration that was supplied when the export job was
-- created.
newOutputDataConfig ::
  OutputDataConfig
newOutputDataConfig =
  OutputDataConfig'
    { s3Configuration =
        Prelude.Nothing
    }

-- | The output data configuration that was supplied when the export job was
-- created.
outputDataConfig_s3Configuration :: Lens.Lens' OutputDataConfig (Prelude.Maybe S3Configuration)
outputDataConfig_s3Configuration = Lens.lens (\OutputDataConfig' {s3Configuration} -> s3Configuration) (\s@OutputDataConfig' {} a -> s {s3Configuration = a} :: OutputDataConfig)

instance Data.FromJSON OutputDataConfig where
  parseJSON =
    Data.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Data..:? "S3Configuration")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Configuration

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf s3Configuration

instance Data.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Configuration" Data..=)
              Prelude.<$> s3Configuration
          ]
      )
