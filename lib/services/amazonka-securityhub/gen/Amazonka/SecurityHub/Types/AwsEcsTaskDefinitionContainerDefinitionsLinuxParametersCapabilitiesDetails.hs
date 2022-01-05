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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
  { -- | The Linux capabilities for the container that are dropped from the
    -- default configuration provided by Docker.
    drop :: Prelude.Maybe [Prelude.Text],
    -- | The Linux capabilities for the container that are added to the default
    -- configuration provided by Docker.
    add :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'drop', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop' - The Linux capabilities for the container that are dropped from the
-- default configuration provided by Docker.
--
-- 'add', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add' - The Linux capabilities for the container that are added to the default
-- configuration provided by Docker.
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
    { drop =
        Prelude.Nothing,
      add =
        Prelude.Nothing
    }

-- | The Linux capabilities for the container that are dropped from the
-- default configuration provided by Docker.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_drop = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {drop} -> drop) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {} a -> s {drop = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Linux capabilities for the container that are added to the default
-- configuration provided by Docker.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails_add = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {add} -> add) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {} a -> s {add = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails'
            Prelude.<$> (x Core..:? "Drop" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Add" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      _salt `Prelude.hashWithSalt` drop
        `Prelude.hashWithSalt` add

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      Prelude.rnf drop `Prelude.seq` Prelude.rnf add

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Drop" Core..=) Prelude.<$> drop,
              ("Add" Core..=) Prelude.<$> add
            ]
        )
