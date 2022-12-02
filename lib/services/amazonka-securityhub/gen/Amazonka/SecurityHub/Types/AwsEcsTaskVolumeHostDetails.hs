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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskVolumeHostDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskVolumeHostDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details on a container instance bind mount host volume.
--
-- /See:/ 'newAwsEcsTaskVolumeHostDetails' smart constructor.
data AwsEcsTaskVolumeHostDetails = AwsEcsTaskVolumeHostDetails'
  { -- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
    -- path on the host container instance that\'s presented to the container.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskVolumeHostDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'awsEcsTaskVolumeHostDetails_sourcePath' - When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that\'s presented to the container.
newAwsEcsTaskVolumeHostDetails ::
  AwsEcsTaskVolumeHostDetails
newAwsEcsTaskVolumeHostDetails =
  AwsEcsTaskVolumeHostDetails'
    { sourcePath =
        Prelude.Nothing
    }

-- | When the @host@ parameter is used, specify a @sourcePath@ to declare the
-- path on the host container instance that\'s presented to the container.
awsEcsTaskVolumeHostDetails_sourcePath :: Lens.Lens' AwsEcsTaskVolumeHostDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskVolumeHostDetails_sourcePath = Lens.lens (\AwsEcsTaskVolumeHostDetails' {sourcePath} -> sourcePath) (\s@AwsEcsTaskVolumeHostDetails' {} a -> s {sourcePath = a} :: AwsEcsTaskVolumeHostDetails)

instance Data.FromJSON AwsEcsTaskVolumeHostDetails where
  parseJSON =
    Data.withObject
      "AwsEcsTaskVolumeHostDetails"
      ( \x ->
          AwsEcsTaskVolumeHostDetails'
            Prelude.<$> (x Data..:? "SourcePath")
      )

instance Prelude.Hashable AwsEcsTaskVolumeHostDetails where
  hashWithSalt _salt AwsEcsTaskVolumeHostDetails' {..} =
    _salt `Prelude.hashWithSalt` sourcePath

instance Prelude.NFData AwsEcsTaskVolumeHostDetails where
  rnf AwsEcsTaskVolumeHostDetails' {..} =
    Prelude.rnf sourcePath

instance Data.ToJSON AwsEcsTaskVolumeHostDetails where
  toJSON AwsEcsTaskVolumeHostDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SourcePath" Data..=) Prelude.<$> sourcePath]
      )
