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
-- Module      : Amazonka.QuickSight.Types.S3Parameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.S3Parameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ManifestFileLocation

-- | The parameters for S3.
--
-- /See:/ 'newS3Parameters' smart constructor.
data S3Parameters = S3Parameters'
  { -- | Use the @RoleArn@ structure to override an account-wide role for a
    -- specific S3 data source. For example, say an account administrator has
    -- turned off all S3 access with an account-wide role. The administrator
    -- can then use @RoleArn@ to bypass the account-wide role and allow S3
    -- access for the single S3 data source that is specified in the structure,
    -- even if the account-wide role forbidding S3 access is still active.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Location of the Amazon S3 manifest file. This is NULL if the manifest
    -- file was uploaded into Amazon QuickSight.
    manifestFileLocation :: ManifestFileLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Parameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 's3Parameters_roleArn' - Use the @RoleArn@ structure to override an account-wide role for a
-- specific S3 data source. For example, say an account administrator has
-- turned off all S3 access with an account-wide role. The administrator
-- can then use @RoleArn@ to bypass the account-wide role and allow S3
-- access for the single S3 data source that is specified in the structure,
-- even if the account-wide role forbidding S3 access is still active.
--
-- 'manifestFileLocation', 's3Parameters_manifestFileLocation' - Location of the Amazon S3 manifest file. This is NULL if the manifest
-- file was uploaded into Amazon QuickSight.
newS3Parameters ::
  -- | 'manifestFileLocation'
  ManifestFileLocation ->
  S3Parameters
newS3Parameters pManifestFileLocation_ =
  S3Parameters'
    { roleArn = Prelude.Nothing,
      manifestFileLocation = pManifestFileLocation_
    }

-- | Use the @RoleArn@ structure to override an account-wide role for a
-- specific S3 data source. For example, say an account administrator has
-- turned off all S3 access with an account-wide role. The administrator
-- can then use @RoleArn@ to bypass the account-wide role and allow S3
-- access for the single S3 data source that is specified in the structure,
-- even if the account-wide role forbidding S3 access is still active.
s3Parameters_roleArn :: Lens.Lens' S3Parameters (Prelude.Maybe Prelude.Text)
s3Parameters_roleArn = Lens.lens (\S3Parameters' {roleArn} -> roleArn) (\s@S3Parameters' {} a -> s {roleArn = a} :: S3Parameters)

-- | Location of the Amazon S3 manifest file. This is NULL if the manifest
-- file was uploaded into Amazon QuickSight.
s3Parameters_manifestFileLocation :: Lens.Lens' S3Parameters ManifestFileLocation
s3Parameters_manifestFileLocation = Lens.lens (\S3Parameters' {manifestFileLocation} -> manifestFileLocation) (\s@S3Parameters' {} a -> s {manifestFileLocation = a} :: S3Parameters)

instance Data.FromJSON S3Parameters where
  parseJSON =
    Data.withObject
      "S3Parameters"
      ( \x ->
          S3Parameters'
            Prelude.<$> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..: "ManifestFileLocation")
      )

instance Prelude.Hashable S3Parameters where
  hashWithSalt _salt S3Parameters' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` manifestFileLocation

instance Prelude.NFData S3Parameters where
  rnf S3Parameters' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf manifestFileLocation

instance Data.ToJSON S3Parameters where
  toJSON S3Parameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just
              ( "ManifestFileLocation"
                  Data..= manifestFileLocation
              )
          ]
      )
