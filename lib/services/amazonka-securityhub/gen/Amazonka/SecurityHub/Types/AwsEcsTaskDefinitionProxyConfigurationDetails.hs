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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails

-- | The configuration details for the App Mesh proxy.
--
-- /See:/ 'newAwsEcsTaskDefinitionProxyConfigurationDetails' smart constructor.
data AwsEcsTaskDefinitionProxyConfigurationDetails = AwsEcsTaskDefinitionProxyConfigurationDetails'
  { -- | The name of the container that will serve as the App Mesh proxy.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The proxy type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The set of network configuration parameters to provide to the Container
    -- Network Interface (CNI) plugin, specified as key-value pairs.
    proxyConfigurationProperties :: Prelude.Maybe [AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionProxyConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'awsEcsTaskDefinitionProxyConfigurationDetails_containerName' - The name of the container that will serve as the App Mesh proxy.
--
-- 'type'', 'awsEcsTaskDefinitionProxyConfigurationDetails_type' - The proxy type.
--
-- 'proxyConfigurationProperties', 'awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties' - The set of network configuration parameters to provide to the Container
-- Network Interface (CNI) plugin, specified as key-value pairs.
newAwsEcsTaskDefinitionProxyConfigurationDetails ::
  AwsEcsTaskDefinitionProxyConfigurationDetails
newAwsEcsTaskDefinitionProxyConfigurationDetails =
  AwsEcsTaskDefinitionProxyConfigurationDetails'
    { containerName =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      proxyConfigurationProperties =
        Prelude.Nothing
    }

-- | The name of the container that will serve as the App Mesh proxy.
awsEcsTaskDefinitionProxyConfigurationDetails_containerName :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationDetails_containerName = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationDetails' {containerName} -> containerName) (\s@AwsEcsTaskDefinitionProxyConfigurationDetails' {} a -> s {containerName = a} :: AwsEcsTaskDefinitionProxyConfigurationDetails)

-- | The proxy type.
awsEcsTaskDefinitionProxyConfigurationDetails_type :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionProxyConfigurationDetails_type = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationDetails' {type'} -> type') (\s@AwsEcsTaskDefinitionProxyConfigurationDetails' {} a -> s {type' = a} :: AwsEcsTaskDefinitionProxyConfigurationDetails)

-- | The set of network configuration parameters to provide to the Container
-- Network Interface (CNI) plugin, specified as key-value pairs.
awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties :: Lens.Lens' AwsEcsTaskDefinitionProxyConfigurationDetails (Prelude.Maybe [AwsEcsTaskDefinitionProxyConfigurationProxyConfigurationPropertiesDetails])
awsEcsTaskDefinitionProxyConfigurationDetails_proxyConfigurationProperties = Lens.lens (\AwsEcsTaskDefinitionProxyConfigurationDetails' {proxyConfigurationProperties} -> proxyConfigurationProperties) (\s@AwsEcsTaskDefinitionProxyConfigurationDetails' {} a -> s {proxyConfigurationProperties = a} :: AwsEcsTaskDefinitionProxyConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsEcsTaskDefinitionProxyConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionProxyConfigurationDetails"
      ( \x ->
          AwsEcsTaskDefinitionProxyConfigurationDetails'
            Prelude.<$> (x Core..:? "ContainerName")
              Prelude.<*> (x Core..:? "Type")
              Prelude.<*> ( x Core..:? "ProxyConfigurationProperties"
                              Core..!= Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionProxyConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionProxyConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` containerName
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` proxyConfigurationProperties

instance
  Prelude.NFData
    AwsEcsTaskDefinitionProxyConfigurationDetails
  where
  rnf
    AwsEcsTaskDefinitionProxyConfigurationDetails' {..} =
      Prelude.rnf containerName
        `Prelude.seq` Prelude.rnf type'
        `Prelude.seq` Prelude.rnf proxyConfigurationProperties

instance
  Core.ToJSON
    AwsEcsTaskDefinitionProxyConfigurationDetails
  where
  toJSON
    AwsEcsTaskDefinitionProxyConfigurationDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ContainerName" Core..=) Prelude.<$> containerName,
              ("Type" Core..=) Prelude.<$> type',
              ("ProxyConfigurationProperties" Core..=)
                Prelude.<$> proxyConfigurationProperties
            ]
        )
