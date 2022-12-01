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
-- Module      : Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcrRepositoryImageScanningConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The image scanning configuration for a repository.
--
-- /See:/ 'newAwsEcrRepositoryImageScanningConfigurationDetails' smart constructor.
data AwsEcrRepositoryImageScanningConfigurationDetails = AwsEcrRepositoryImageScanningConfigurationDetails'
  { -- | Whether to scan images after they are pushed to a repository.
    scanOnPush :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcrRepositoryImageScanningConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanOnPush', 'awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush' - Whether to scan images after they are pushed to a repository.
newAwsEcrRepositoryImageScanningConfigurationDetails ::
  AwsEcrRepositoryImageScanningConfigurationDetails
newAwsEcrRepositoryImageScanningConfigurationDetails =
  AwsEcrRepositoryImageScanningConfigurationDetails'
    { scanOnPush =
        Prelude.Nothing
    }

-- | Whether to scan images after they are pushed to a repository.
awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush :: Lens.Lens' AwsEcrRepositoryImageScanningConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcrRepositoryImageScanningConfigurationDetails_scanOnPush = Lens.lens (\AwsEcrRepositoryImageScanningConfigurationDetails' {scanOnPush} -> scanOnPush) (\s@AwsEcrRepositoryImageScanningConfigurationDetails' {} a -> s {scanOnPush = a} :: AwsEcrRepositoryImageScanningConfigurationDetails)

instance
  Core.FromJSON
    AwsEcrRepositoryImageScanningConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcrRepositoryImageScanningConfigurationDetails"
      ( \x ->
          AwsEcrRepositoryImageScanningConfigurationDetails'
            Prelude.<$> (x Core..:? "ScanOnPush")
      )

instance
  Prelude.Hashable
    AwsEcrRepositoryImageScanningConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcrRepositoryImageScanningConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` scanOnPush

instance
  Prelude.NFData
    AwsEcrRepositoryImageScanningConfigurationDetails
  where
  rnf
    AwsEcrRepositoryImageScanningConfigurationDetails' {..} =
      Prelude.rnf scanOnPush

instance
  Core.ToJSON
    AwsEcrRepositoryImageScanningConfigurationDetails
  where
  toJSON
    AwsEcrRepositoryImageScanningConfigurationDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [("ScanOnPush" Core..=) Prelude.<$> scanOnPush]
        )
