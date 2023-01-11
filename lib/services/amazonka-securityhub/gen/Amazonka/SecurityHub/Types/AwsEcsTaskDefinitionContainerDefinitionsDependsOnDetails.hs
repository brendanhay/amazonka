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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The dependency condition of the dependent container. Indicates the
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
    condition :: Prelude.Maybe Prelude.Text,
    -- | The name of the dependent container.
    containerName :: Prelude.Maybe Prelude.Text
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
--
-- 'containerName', 'awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName' - The name of the dependent container.
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
newAwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails =
  AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
    { condition =
        Prelude.Nothing,
      containerName =
        Prelude.Nothing
    }

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

-- | The name of the dependent container.
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDependsOnDetails_containerName = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {containerName} -> containerName) (\s@AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {} a -> s {containerName = a} :: AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails'
            Prelude.<$> (x Data..:? "Condition")
              Prelude.<*> (x Data..:? "ContainerName")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      _salt `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` containerName

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Prelude.rnf condition
        `Prelude.seq` Prelude.rnf containerName

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Condition" Data..=) Prelude.<$> condition,
              ("ContainerName" Data..=) Prelude.<$> containerName
            ]
        )
