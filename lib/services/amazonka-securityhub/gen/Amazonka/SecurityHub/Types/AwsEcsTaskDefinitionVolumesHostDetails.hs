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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesHostDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesHostDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a bind mount host volume.
--
-- /See:/ 'newAwsEcsTaskDefinitionVolumesHostDetails' smart constructor.
data AwsEcsTaskDefinitionVolumesHostDetails = AwsEcsTaskDefinitionVolumesHostDetails'
  { -- | The path on the host container instance that is presented to the
    -- container.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionVolumesHostDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'awsEcsTaskDefinitionVolumesHostDetails_sourcePath' - The path on the host container instance that is presented to the
-- container.
newAwsEcsTaskDefinitionVolumesHostDetails ::
  AwsEcsTaskDefinitionVolumesHostDetails
newAwsEcsTaskDefinitionVolumesHostDetails =
  AwsEcsTaskDefinitionVolumesHostDetails'
    { sourcePath =
        Prelude.Nothing
    }

-- | The path on the host container instance that is presented to the
-- container.
awsEcsTaskDefinitionVolumesHostDetails_sourcePath :: Lens.Lens' AwsEcsTaskDefinitionVolumesHostDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesHostDetails_sourcePath = Lens.lens (\AwsEcsTaskDefinitionVolumesHostDetails' {sourcePath} -> sourcePath) (\s@AwsEcsTaskDefinitionVolumesHostDetails' {} a -> s {sourcePath = a} :: AwsEcsTaskDefinitionVolumesHostDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionVolumesHostDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionVolumesHostDetails"
      ( \x ->
          AwsEcsTaskDefinitionVolumesHostDetails'
            Prelude.<$> (x Data..:? "SourcePath")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionVolumesHostDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionVolumesHostDetails' {..} =
      _salt `Prelude.hashWithSalt` sourcePath

instance
  Prelude.NFData
    AwsEcsTaskDefinitionVolumesHostDetails
  where
  rnf AwsEcsTaskDefinitionVolumesHostDetails' {..} =
    Prelude.rnf sourcePath

instance
  Data.ToJSON
    AwsEcsTaskDefinitionVolumesHostDetails
  where
  toJSON AwsEcsTaskDefinitionVolumesHostDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SourcePath" Data..=) Prelude.<$> sourcePath]
      )
