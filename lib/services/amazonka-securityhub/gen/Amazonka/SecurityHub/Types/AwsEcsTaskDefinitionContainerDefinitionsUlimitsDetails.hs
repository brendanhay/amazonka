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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A ulimit to set in the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails = AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails'
  { -- | The type of the ulimit. Valid values are as follows:
    --
    -- -   @core@
    --
    -- -   @cpu@
    --
    -- -   @data@
    --
    -- -   @fsize@
    --
    -- -   @locks@
    --
    -- -   @memlock@
    --
    -- -   @msgqueue@
    --
    -- -   @nice@
    --
    -- -   @nofile@
    --
    -- -   @nproc@
    --
    -- -   @rss@
    --
    -- -   @rtprio@
    --
    -- -   @rttime@
    --
    -- -   @sigpending@
    --
    -- -   @stack@
    name :: Prelude.Maybe Prelude.Text,
    -- | The soft limit for the ulimit type.
    softLimit :: Prelude.Maybe Prelude.Int,
    -- | The hard limit for the ulimit type.
    hardLimit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name' - The type of the ulimit. Valid values are as follows:
--
-- -   @core@
--
-- -   @cpu@
--
-- -   @data@
--
-- -   @fsize@
--
-- -   @locks@
--
-- -   @memlock@
--
-- -   @msgqueue@
--
-- -   @nice@
--
-- -   @nofile@
--
-- -   @nproc@
--
-- -   @rss@
--
-- -   @rtprio@
--
-- -   @rttime@
--
-- -   @sigpending@
--
-- -   @stack@
--
-- 'softLimit', 'awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit' - The soft limit for the ulimit type.
--
-- 'hardLimit', 'awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit' - The hard limit for the ulimit type.
newAwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
newAwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails'
    { name =
        Prelude.Nothing,
      softLimit =
        Prelude.Nothing,
      hardLimit =
        Prelude.Nothing
    }

-- | The type of the ulimit. Valid values are as follows:
--
-- -   @core@
--
-- -   @cpu@
--
-- -   @data@
--
-- -   @fsize@
--
-- -   @locks@
--
-- -   @memlock@
--
-- -   @msgqueue@
--
-- -   @nice@
--
-- -   @nofile@
--
-- -   @nproc@
--
-- -   @rss@
--
-- -   @rtprio@
--
-- -   @rttime@
--
-- -   @sigpending@
--
-- -   @stack@
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_name = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {name} -> name) (\s@AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails)

-- | The soft limit for the ulimit type.
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_softLimit = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {softLimit} -> softLimit) (\s@AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {} a -> s {softLimit = a} :: AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails)

-- | The hard limit for the ulimit type.
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsUlimitsDetails_hardLimit = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {hardLimit} -> hardLimit) (\s@AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {} a -> s {hardLimit = a} :: AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails'
            Prelude.<$> (x Data..:? "Name")
              Prelude.<*> (x Data..:? "SoftLimit")
              Prelude.<*> (x Data..:? "HardLimit")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` softLimit
        `Prelude.hashWithSalt` hardLimit

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf softLimit
        `Prelude.seq` Prelude.rnf hardLimit

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              ("SoftLimit" Data..=) Prelude.<$> softLimit,
              ("HardLimit" Data..=) Prelude.<$> hardLimit
            ]
        )
