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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A port mapping for the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails = AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
  { -- | The port number on the container that is bound to the user-specified or
    -- automatically assigned host port.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | The port number on the container instance to reserve for the container.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The protocol used for the port mapping. The default is @tcp@.
    protocol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPort', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort' - The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
--
-- 'hostPort', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort' - The port number on the container instance to reserve for the container.
--
-- 'protocol', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol' - The protocol used for the port mapping. The default is @tcp@.
newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
    { containerPort =
        Prelude.Nothing,
      hostPort =
        Prelude.Nothing,
      protocol =
        Prelude.Nothing
    }

-- | The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {containerPort} -> containerPort) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {containerPort = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

-- | The port number on the container instance to reserve for the container.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {hostPort} -> hostPort) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {hostPort = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

-- | The protocol used for the port mapping. The default is @tcp@.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {protocol} -> protocol) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {protocol = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
            Prelude.<$> (x Data..:? "ContainerPort")
            Prelude.<*> (x Data..:? "HostPort")
            Prelude.<*> (x Data..:? "Protocol")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` containerPort
        `Prelude.hashWithSalt` hostPort
        `Prelude.hashWithSalt` protocol

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {..} =
      Prelude.rnf containerPort
        `Prelude.seq` Prelude.rnf hostPort
        `Prelude.seq` Prelude.rnf protocol

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ContainerPort" Data..=) Prelude.<$> containerPort,
              ("HostPort" Data..=) Prelude.<$> hostPort,
              ("Protocol" Data..=) Prelude.<$> protocol
            ]
        )
