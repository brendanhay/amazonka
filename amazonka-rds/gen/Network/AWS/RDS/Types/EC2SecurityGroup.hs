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
-- Module      : Network.AWS.RDS.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EC2SecurityGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This data type is used as a response element in the following actions:
--
-- -   @AuthorizeDBSecurityGroupIngress@
--
-- -   @DescribeDBSecurityGroups@
--
-- -   @RevokeDBSecurityGroupIngress@
--
-- /See:/ 'newEC2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { -- | Provides the status of the EC2 security group. Status can be
    -- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
    status :: Prelude.Maybe Prelude.Text,
    -- | Specifies the AWS ID of the owner of the EC2 security group specified in
    -- the @EC2SecurityGroupName@ field.
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the id of the EC2 security group.
    eC2SecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the EC2 security group.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EC2SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'eC2SecurityGroup_status' - Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
--
-- 'eC2SecurityGroupOwnerId', 'eC2SecurityGroup_eC2SecurityGroupOwnerId' - Specifies the AWS ID of the owner of the EC2 security group specified in
-- the @EC2SecurityGroupName@ field.
--
-- 'eC2SecurityGroupId', 'eC2SecurityGroup_eC2SecurityGroupId' - Specifies the id of the EC2 security group.
--
-- 'eC2SecurityGroupName', 'eC2SecurityGroup_eC2SecurityGroupName' - Specifies the name of the EC2 security group.
newEC2SecurityGroup ::
  EC2SecurityGroup
newEC2SecurityGroup =
  EC2SecurityGroup'
    { status = Prelude.Nothing,
      eC2SecurityGroupOwnerId = Prelude.Nothing,
      eC2SecurityGroupId = Prelude.Nothing,
      eC2SecurityGroupName = Prelude.Nothing
    }

-- | Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
eC2SecurityGroup_status :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_status = Lens.lens (\EC2SecurityGroup' {status} -> status) (\s@EC2SecurityGroup' {} a -> s {status = a} :: EC2SecurityGroup)

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the @EC2SecurityGroupName@ field.
eC2SecurityGroup_eC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupOwnerId = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupOwnerId = a} :: EC2SecurityGroup)

-- | Specifies the id of the EC2 security group.
eC2SecurityGroup_eC2SecurityGroupId :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupId = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupId} -> eC2SecurityGroupId) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupId = a} :: EC2SecurityGroup)

-- | Specifies the name of the EC2 security group.
eC2SecurityGroup_eC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupName = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupName = a} :: EC2SecurityGroup)

instance Prelude.FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "EC2SecurityGroupOwnerId")
      Prelude.<*> (x Prelude..@? "EC2SecurityGroupId")
      Prelude.<*> (x Prelude..@? "EC2SecurityGroupName")

instance Prelude.Hashable EC2SecurityGroup

instance Prelude.NFData EC2SecurityGroup
