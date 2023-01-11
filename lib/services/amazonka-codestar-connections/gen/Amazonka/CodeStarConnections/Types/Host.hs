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
-- Module      : Amazonka.CodeStarConnections.Types.Host
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types.Host where

import Amazonka.CodeStarConnections.Types.ProviderType
import Amazonka.CodeStarConnections.Types.VpcConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource that represents the infrastructure where a third-party
-- provider is installed. The host is used when you create connections to
-- an installed third-party provider type, such as GitHub Enterprise
-- Server. You create one host for all connections to that provider.
--
-- A host created through the CLI or the SDK is in \`PENDING\` status by
-- default. You can make its status \`AVAILABLE\` by setting up the host in
-- the console.
--
-- /See:/ 'newHost' smart constructor.
data Host = Host'
  { -- | The Amazon Resource Name (ARN) of the host.
    hostArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the host.
    name :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the infrastructure where your provider type is
    -- installed.
    providerEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the installed provider to be associated with your
    -- connection. The host resource represents the infrastructure where your
    -- provider type is installed. The valid provider type is GitHub Enterprise
    -- Server.
    providerType :: Prelude.Maybe ProviderType,
    -- | The status of the host, such as PENDING, AVAILABLE, VPC_CONFIG_DELETING,
    -- VPC_CONFIG_INITIALIZING, and VPC_CONFIG_FAILED_INITIALIZATION.
    status :: Prelude.Maybe Prelude.Text,
    -- | The status description for the host.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The VPC configuration provisioned for the host.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Host' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostArn', 'host_hostArn' - The Amazon Resource Name (ARN) of the host.
--
-- 'name', 'host_name' - The name of the host.
--
-- 'providerEndpoint', 'host_providerEndpoint' - The endpoint of the infrastructure where your provider type is
-- installed.
--
-- 'providerType', 'host_providerType' - The name of the installed provider to be associated with your
-- connection. The host resource represents the infrastructure where your
-- provider type is installed. The valid provider type is GitHub Enterprise
-- Server.
--
-- 'status', 'host_status' - The status of the host, such as PENDING, AVAILABLE, VPC_CONFIG_DELETING,
-- VPC_CONFIG_INITIALIZING, and VPC_CONFIG_FAILED_INITIALIZATION.
--
-- 'statusMessage', 'host_statusMessage' - The status description for the host.
--
-- 'vpcConfiguration', 'host_vpcConfiguration' - The VPC configuration provisioned for the host.
newHost ::
  Host
newHost =
  Host'
    { hostArn = Prelude.Nothing,
      name = Prelude.Nothing,
      providerEndpoint = Prelude.Nothing,
      providerType = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      vpcConfiguration = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the host.
host_hostArn :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_hostArn = Lens.lens (\Host' {hostArn} -> hostArn) (\s@Host' {} a -> s {hostArn = a} :: Host)

-- | The name of the host.
host_name :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_name = Lens.lens (\Host' {name} -> name) (\s@Host' {} a -> s {name = a} :: Host)

-- | The endpoint of the infrastructure where your provider type is
-- installed.
host_providerEndpoint :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_providerEndpoint = Lens.lens (\Host' {providerEndpoint} -> providerEndpoint) (\s@Host' {} a -> s {providerEndpoint = a} :: Host)

-- | The name of the installed provider to be associated with your
-- connection. The host resource represents the infrastructure where your
-- provider type is installed. The valid provider type is GitHub Enterprise
-- Server.
host_providerType :: Lens.Lens' Host (Prelude.Maybe ProviderType)
host_providerType = Lens.lens (\Host' {providerType} -> providerType) (\s@Host' {} a -> s {providerType = a} :: Host)

-- | The status of the host, such as PENDING, AVAILABLE, VPC_CONFIG_DELETING,
-- VPC_CONFIG_INITIALIZING, and VPC_CONFIG_FAILED_INITIALIZATION.
host_status :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_status = Lens.lens (\Host' {status} -> status) (\s@Host' {} a -> s {status = a} :: Host)

-- | The status description for the host.
host_statusMessage :: Lens.Lens' Host (Prelude.Maybe Prelude.Text)
host_statusMessage = Lens.lens (\Host' {statusMessage} -> statusMessage) (\s@Host' {} a -> s {statusMessage = a} :: Host)

-- | The VPC configuration provisioned for the host.
host_vpcConfiguration :: Lens.Lens' Host (Prelude.Maybe VpcConfiguration)
host_vpcConfiguration = Lens.lens (\Host' {vpcConfiguration} -> vpcConfiguration) (\s@Host' {} a -> s {vpcConfiguration = a} :: Host)

instance Data.FromJSON Host where
  parseJSON =
    Data.withObject
      "Host"
      ( \x ->
          Host'
            Prelude.<$> (x Data..:? "HostArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProviderEndpoint")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "VpcConfiguration")
      )

instance Prelude.Hashable Host where
  hashWithSalt _salt Host' {..} =
    _salt `Prelude.hashWithSalt` hostArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` providerEndpoint
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` vpcConfiguration

instance Prelude.NFData Host where
  rnf Host' {..} =
    Prelude.rnf hostArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf providerEndpoint
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf vpcConfiguration
