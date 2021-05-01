{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EKS.Types.RemoteAccessConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.RemoteAccessConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the remote access configuration for the managed
-- node group.
--
-- /See:/ 'newRemoteAccessConfig' smart constructor.
data RemoteAccessConfig = RemoteAccessConfig'
  { -- | The Amazon EC2 SSH key that provides access for SSH communication with
    -- the nodes in the managed node group. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
    -- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
    ec2SshKey :: Prelude.Maybe Prelude.Text,
    -- | The security groups that are allowed SSH access (port 22) to the nodes.
    -- If you specify an Amazon EC2 SSH key but do not specify a source
    -- security group when you create a managed node group, then port 22 on the
    -- nodes is opened to the internet (0.0.0.0\/0). For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for Your VPC>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    sourceSecurityGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoteAccessConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2SshKey', 'remoteAccessConfig_ec2SshKey' - The Amazon EC2 SSH key that provides access for SSH communication with
-- the nodes in the managed node group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
--
-- 'sourceSecurityGroups', 'remoteAccessConfig_sourceSecurityGroups' - The security groups that are allowed SSH access (port 22) to the nodes.
-- If you specify an Amazon EC2 SSH key but do not specify a source
-- security group when you create a managed node group, then port 22 on the
-- nodes is opened to the internet (0.0.0.0\/0). For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
newRemoteAccessConfig ::
  RemoteAccessConfig
newRemoteAccessConfig =
  RemoteAccessConfig'
    { ec2SshKey = Prelude.Nothing,
      sourceSecurityGroups = Prelude.Nothing
    }

-- | The Amazon EC2 SSH key that provides access for SSH communication with
-- the nodes in the managed node group. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
remoteAccessConfig_ec2SshKey :: Lens.Lens' RemoteAccessConfig (Prelude.Maybe Prelude.Text)
remoteAccessConfig_ec2SshKey = Lens.lens (\RemoteAccessConfig' {ec2SshKey} -> ec2SshKey) (\s@RemoteAccessConfig' {} a -> s {ec2SshKey = a} :: RemoteAccessConfig)

-- | The security groups that are allowed SSH access (port 22) to the nodes.
-- If you specify an Amazon EC2 SSH key but do not specify a source
-- security group when you create a managed node group, then port 22 on the
-- nodes is opened to the internet (0.0.0.0\/0). For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
remoteAccessConfig_sourceSecurityGroups :: Lens.Lens' RemoteAccessConfig (Prelude.Maybe [Prelude.Text])
remoteAccessConfig_sourceSecurityGroups = Lens.lens (\RemoteAccessConfig' {sourceSecurityGroups} -> sourceSecurityGroups) (\s@RemoteAccessConfig' {} a -> s {sourceSecurityGroups = a} :: RemoteAccessConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON RemoteAccessConfig where
  parseJSON =
    Prelude.withObject
      "RemoteAccessConfig"
      ( \x ->
          RemoteAccessConfig'
            Prelude.<$> (x Prelude..:? "ec2SshKey")
            Prelude.<*> ( x Prelude..:? "sourceSecurityGroups"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RemoteAccessConfig

instance Prelude.NFData RemoteAccessConfig

instance Prelude.ToJSON RemoteAccessConfig where
  toJSON RemoteAccessConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ec2SshKey" Prelude..=) Prelude.<$> ec2SshKey,
            ("sourceSecurityGroups" Prelude..=)
              Prelude.<$> sourceSecurityGroups
          ]
      )
