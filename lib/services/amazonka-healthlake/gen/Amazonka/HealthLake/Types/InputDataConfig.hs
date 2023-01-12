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
-- Module      : Amazonka.HealthLake.Types.InputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.InputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input properties for an import job.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | The S3Uri is the user specified S3 location of the FHIR data to be
    -- imported into Amazon HealthLake.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'inputDataConfig_s3Uri' - The S3Uri is the user specified S3 location of the FHIR data to be
-- imported into Amazon HealthLake.
newInputDataConfig ::
  InputDataConfig
newInputDataConfig =
  InputDataConfig' {s3Uri = Prelude.Nothing}

-- | The S3Uri is the user specified S3 location of the FHIR data to be
-- imported into Amazon HealthLake.
inputDataConfig_s3Uri :: Lens.Lens' InputDataConfig (Prelude.Maybe Prelude.Text)
inputDataConfig_s3Uri = Lens.lens (\InputDataConfig' {s3Uri} -> s3Uri) (\s@InputDataConfig' {} a -> s {s3Uri = a} :: InputDataConfig)

instance Data.FromJSON InputDataConfig where
  parseJSON =
    Data.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig' Prelude.<$> (x Data..:? "S3Uri")
      )

instance Prelude.Hashable InputDataConfig where
  hashWithSalt _salt InputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData InputDataConfig where
  rnf InputDataConfig' {..} = Prelude.rnf s3Uri

instance Data.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Uri" Data..=) Prelude.<$> s3Uri]
      )
