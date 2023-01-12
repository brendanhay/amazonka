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
-- Module      : Amazonka.IoTSiteWise.Types.JobConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.JobConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.FileFormat
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of a job, such as the file format
-- used to save data in Amazon S3.
--
-- /See:/ 'newJobConfiguration' smart constructor.
data JobConfiguration = JobConfiguration'
  { -- | The file format of the data in Amazon S3.
    fileFormat :: FileFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileFormat', 'jobConfiguration_fileFormat' - The file format of the data in Amazon S3.
newJobConfiguration ::
  -- | 'fileFormat'
  FileFormat ->
  JobConfiguration
newJobConfiguration pFileFormat_ =
  JobConfiguration' {fileFormat = pFileFormat_}

-- | The file format of the data in Amazon S3.
jobConfiguration_fileFormat :: Lens.Lens' JobConfiguration FileFormat
jobConfiguration_fileFormat = Lens.lens (\JobConfiguration' {fileFormat} -> fileFormat) (\s@JobConfiguration' {} a -> s {fileFormat = a} :: JobConfiguration)

instance Data.FromJSON JobConfiguration where
  parseJSON =
    Data.withObject
      "JobConfiguration"
      ( \x ->
          JobConfiguration'
            Prelude.<$> (x Data..: "fileFormat")
      )

instance Prelude.Hashable JobConfiguration where
  hashWithSalt _salt JobConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fileFormat

instance Prelude.NFData JobConfiguration where
  rnf JobConfiguration' {..} = Prelude.rnf fileFormat

instance Data.ToJSON JobConfiguration where
  toJSON JobConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fileFormat" Data..= fileFormat)]
      )
