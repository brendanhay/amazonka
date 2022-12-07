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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A dependency that is defined for container startup and shutdown.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails = AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
  { -- | The name of the dependent container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The dependency condition of the dependent container. Indicates the
    -- required status of the dependent container before the current container
    -- can start. Valid values are as follows:
    --
    -- -   @COMPLETE@
    --
    -- -   @HEALTHY@
    --
    -- -   @SUCCESS@
    --
    -- -   @START@
    condition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName' - The name of the dependent container.
--
-- 'condition', 'awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition' - The dependency condition of the dependent container. Indicates the
-- required status of the dependent container before the current container
-- can start. Valid values are as follows:
--
-- -   @COMPLETE@
--
-- -   @HEALTHY@
--
-- -   @SUCCESS@
--
-- -   @START@
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails =
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
    { containerName =
        Prelude.Nothing,
      condition =
        Prelude.Nothing
    }

-- | The name of the dependent container.
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {containerName} -> containerName) (\s@AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {} a -> s {containerName = a} :: AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails)

-- | The dependency condition of the dependent container. Indicates the
-- required status of the dependent container before the current container
-- can start. Valid values are as follows:
--
-- -   @COMPLETE@
--
-- -   @HEALTHY@
--
-- -   @SUCCESS@
--
-- -   @START@
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_condition = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {condition} -> condition) (\s@AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {} a -> s {condition = a} :: AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
            Prelude.<$> (x Data..:? "ContainerName")
              Prelude.<*> (x Data..:? "Condition")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      _salt `Prelude.hashWithSalt` containerName
        `Prelude.hashWithSalt` condition

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Prelude.rnf containerName
        `Prelude.seq` Prelude.rnf condition

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ContainerName" Data..=) Prelude.<$> containerName,
              ("Condition" Data..=) Prelude.<$> condition
            ]
        )
