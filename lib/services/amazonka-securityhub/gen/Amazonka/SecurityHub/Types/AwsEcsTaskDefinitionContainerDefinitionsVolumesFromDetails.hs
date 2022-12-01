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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A data volume to mount from another container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails = AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails'
  { -- | Whether the container has read-only access to the volume.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of another container within the same task definition from which
    -- to mount volumes.
    sourceContainer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOnly', 'awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly' - Whether the container has read-only access to the volume.
--
-- 'sourceContainer', 'awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer' - The name of another container within the same task definition from which
-- to mount volumes.
newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
newAwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails =
  AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails'
    { readOnly =
        Prelude.Nothing,
      sourceContainer =
        Prelude.Nothing
    }

-- | Whether the container has read-only access to the volume.
awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_readOnly = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {readOnly} -> readOnly) (\s@AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {} a -> s {readOnly = a} :: AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails)

-- | The name of another container within the same task definition from which
-- to mount volumes.
awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails_sourceContainer = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {sourceContainer} -> sourceContainer) (\s@AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {} a -> s {sourceContainer = a} :: AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails'
            Prelude.<$> (x Core..:? "ReadOnly")
              Prelude.<*> (x Core..:? "SourceContainer")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {..} =
      _salt `Prelude.hashWithSalt` readOnly
        `Prelude.hashWithSalt` sourceContainer

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {..} =
      Prelude.rnf readOnly
        `Prelude.seq` Prelude.rnf sourceContainer

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ReadOnly" Core..=) Prelude.<$> readOnly,
              ("SourceContainer" Core..=)
                Prelude.<$> sourceContainer
            ]
        )
