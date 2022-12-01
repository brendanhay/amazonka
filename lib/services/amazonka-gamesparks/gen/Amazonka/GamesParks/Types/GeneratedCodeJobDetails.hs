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
-- Module      : Amazonka.GamesParks.Types.GeneratedCodeJobDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.GeneratedCodeJobDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types.GeneratedCodeJobState
import qualified Amazonka.Prelude as Prelude

-- | Details about a generated code job.
--
-- /See:/ 'newGeneratedCodeJobDetails' smart constructor.
data GeneratedCodeJobDetails = GeneratedCodeJobDetails'
  { -- | The expiration date and time for the download URL.
    --
    -- The download URL us guaranteed to be available until at least this time.
    expirationTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier for the generated code job.
    generatedCodeJobId :: Prelude.Maybe Prelude.Text,
    -- | The status of the generated code job
    status :: Prelude.Maybe GeneratedCodeJobState,
    -- | The description of the generated code job.
    description :: Prelude.Maybe Prelude.Text,
    -- | A presigned URL that can be used to download the generated code.
    s3Url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratedCodeJobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationTime', 'generatedCodeJobDetails_expirationTime' - The expiration date and time for the download URL.
--
-- The download URL us guaranteed to be available until at least this time.
--
-- 'generatedCodeJobId', 'generatedCodeJobDetails_generatedCodeJobId' - The identifier for the generated code job.
--
-- 'status', 'generatedCodeJobDetails_status' - The status of the generated code job
--
-- 'description', 'generatedCodeJobDetails_description' - The description of the generated code job.
--
-- 's3Url', 'generatedCodeJobDetails_s3Url' - A presigned URL that can be used to download the generated code.
newGeneratedCodeJobDetails ::
  GeneratedCodeJobDetails
newGeneratedCodeJobDetails =
  GeneratedCodeJobDetails'
    { expirationTime =
        Prelude.Nothing,
      generatedCodeJobId = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      s3Url = Prelude.Nothing
    }

-- | The expiration date and time for the download URL.
--
-- The download URL us guaranteed to be available until at least this time.
generatedCodeJobDetails_expirationTime :: Lens.Lens' GeneratedCodeJobDetails (Prelude.Maybe Prelude.UTCTime)
generatedCodeJobDetails_expirationTime = Lens.lens (\GeneratedCodeJobDetails' {expirationTime} -> expirationTime) (\s@GeneratedCodeJobDetails' {} a -> s {expirationTime = a} :: GeneratedCodeJobDetails) Prelude.. Lens.mapping Core._Time

-- | The identifier for the generated code job.
generatedCodeJobDetails_generatedCodeJobId :: Lens.Lens' GeneratedCodeJobDetails (Prelude.Maybe Prelude.Text)
generatedCodeJobDetails_generatedCodeJobId = Lens.lens (\GeneratedCodeJobDetails' {generatedCodeJobId} -> generatedCodeJobId) (\s@GeneratedCodeJobDetails' {} a -> s {generatedCodeJobId = a} :: GeneratedCodeJobDetails)

-- | The status of the generated code job
generatedCodeJobDetails_status :: Lens.Lens' GeneratedCodeJobDetails (Prelude.Maybe GeneratedCodeJobState)
generatedCodeJobDetails_status = Lens.lens (\GeneratedCodeJobDetails' {status} -> status) (\s@GeneratedCodeJobDetails' {} a -> s {status = a} :: GeneratedCodeJobDetails)

-- | The description of the generated code job.
generatedCodeJobDetails_description :: Lens.Lens' GeneratedCodeJobDetails (Prelude.Maybe Prelude.Text)
generatedCodeJobDetails_description = Lens.lens (\GeneratedCodeJobDetails' {description} -> description) (\s@GeneratedCodeJobDetails' {} a -> s {description = a} :: GeneratedCodeJobDetails)

-- | A presigned URL that can be used to download the generated code.
generatedCodeJobDetails_s3Url :: Lens.Lens' GeneratedCodeJobDetails (Prelude.Maybe Prelude.Text)
generatedCodeJobDetails_s3Url = Lens.lens (\GeneratedCodeJobDetails' {s3Url} -> s3Url) (\s@GeneratedCodeJobDetails' {} a -> s {s3Url = a} :: GeneratedCodeJobDetails)

instance Core.FromJSON GeneratedCodeJobDetails where
  parseJSON =
    Core.withObject
      "GeneratedCodeJobDetails"
      ( \x ->
          GeneratedCodeJobDetails'
            Prelude.<$> (x Core..:? "ExpirationTime")
            Prelude.<*> (x Core..:? "GeneratedCodeJobId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "S3Url")
      )

instance Prelude.Hashable GeneratedCodeJobDetails where
  hashWithSalt _salt GeneratedCodeJobDetails' {..} =
    _salt `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` generatedCodeJobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` s3Url

instance Prelude.NFData GeneratedCodeJobDetails where
  rnf GeneratedCodeJobDetails' {..} =
    Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf generatedCodeJobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf s3Url
