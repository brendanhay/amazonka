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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskVolumeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskVolumeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskVolumeHostDetails

-- | Provides information about a data volume that\'s used in a task
-- definition.
--
-- /See:/ 'newAwsEcsTaskVolumeDetails' smart constructor.
data AwsEcsTaskVolumeDetails = AwsEcsTaskVolumeDetails'
  { -- | This parameter is specified when you use bind mount host volumes. The
    -- contents of the @host@ parameter determine whether your bind mount host
    -- volume persists on the host container instance and where it\'s stored.
    host :: Prelude.Maybe AwsEcsTaskVolumeHostDetails,
    -- | The name of the volume. Up to 255 letters (uppercase and lowercase),
    -- numbers, underscores, and hyphens are allowed. This name is referenced
    -- in the @sourceVolume@ parameter of container definition @mountPoints@.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskVolumeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'awsEcsTaskVolumeDetails_host' - This parameter is specified when you use bind mount host volumes. The
-- contents of the @host@ parameter determine whether your bind mount host
-- volume persists on the host container instance and where it\'s stored.
--
-- 'name', 'awsEcsTaskVolumeDetails_name' - The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. This name is referenced
-- in the @sourceVolume@ parameter of container definition @mountPoints@.
newAwsEcsTaskVolumeDetails ::
  AwsEcsTaskVolumeDetails
newAwsEcsTaskVolumeDetails =
  AwsEcsTaskVolumeDetails'
    { host = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | This parameter is specified when you use bind mount host volumes. The
-- contents of the @host@ parameter determine whether your bind mount host
-- volume persists on the host container instance and where it\'s stored.
awsEcsTaskVolumeDetails_host :: Lens.Lens' AwsEcsTaskVolumeDetails (Prelude.Maybe AwsEcsTaskVolumeHostDetails)
awsEcsTaskVolumeDetails_host = Lens.lens (\AwsEcsTaskVolumeDetails' {host} -> host) (\s@AwsEcsTaskVolumeDetails' {} a -> s {host = a} :: AwsEcsTaskVolumeDetails)

-- | The name of the volume. Up to 255 letters (uppercase and lowercase),
-- numbers, underscores, and hyphens are allowed. This name is referenced
-- in the @sourceVolume@ parameter of container definition @mountPoints@.
awsEcsTaskVolumeDetails_name :: Lens.Lens' AwsEcsTaskVolumeDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskVolumeDetails_name = Lens.lens (\AwsEcsTaskVolumeDetails' {name} -> name) (\s@AwsEcsTaskVolumeDetails' {} a -> s {name = a} :: AwsEcsTaskVolumeDetails)

instance Data.FromJSON AwsEcsTaskVolumeDetails where
  parseJSON =
    Data.withObject
      "AwsEcsTaskVolumeDetails"
      ( \x ->
          AwsEcsTaskVolumeDetails'
            Prelude.<$> (x Data..:? "Host") Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable AwsEcsTaskVolumeDetails where
  hashWithSalt _salt AwsEcsTaskVolumeDetails' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` name

instance Prelude.NFData AwsEcsTaskVolumeDetails where
  rnf AwsEcsTaskVolumeDetails' {..} =
    Prelude.rnf host `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AwsEcsTaskVolumeDetails where
  toJSON AwsEcsTaskVolumeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Host" Data..=) Prelude.<$> host,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
