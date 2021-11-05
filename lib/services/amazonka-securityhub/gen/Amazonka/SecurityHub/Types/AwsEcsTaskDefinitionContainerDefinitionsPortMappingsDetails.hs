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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A port mapping for the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails = AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
  { -- | The protocol used for the port mapping. The default is @tcp@.
    protocol :: Prelude.Maybe Prelude.Text,
    -- | The port number on the container instance to reserve for the container.
    hostPort :: Prelude.Maybe Prelude.Int,
    -- | The port number on the container that is bound to the user-specified or
    -- automatically assigned host port.
    containerPort :: Prelude.Maybe Prelude.Int
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
-- 'protocol', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol' - The protocol used for the port mapping. The default is @tcp@.
--
-- 'hostPort', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort' - The port number on the container instance to reserve for the container.
--
-- 'containerPort', 'awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort' - The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
newAwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
    { protocol =
        Prelude.Nothing,
      hostPort =
        Prelude.Nothing,
      containerPort =
        Prelude.Nothing
    }

-- | The protocol used for the port mapping. The default is @tcp@.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_protocol = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {protocol} -> protocol) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {protocol = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

-- | The port number on the container instance to reserve for the container.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_hostPort = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {hostPort} -> hostPort) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {hostPort = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

-- | The port number on the container that is bound to the user-specified or
-- automatically assigned host port.
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails_containerPort = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {containerPort} -> containerPort) (\s@AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {} a -> s {containerPort = a} :: AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails'
            Prelude.<$> (x Core..:? "Protocol")
              Prelude.<*> (x Core..:? "HostPort")
              Prelude.<*> (x Core..:? "ContainerPort")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Protocol" Core..=) Prelude.<$> protocol,
              ("HostPort" Core..=) Prelude.<$> hostPort,
              ("ContainerPort" Core..=) Prelude.<$> containerPort
            ]
        )
