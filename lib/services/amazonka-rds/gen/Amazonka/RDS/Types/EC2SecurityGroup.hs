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
-- Module      : Amazonka.RDS.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.EC2SecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | Specifies the id of the EC2 security group.
    eC2SecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the EC2 security group.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Web Services ID of the owner of the EC2 security
    -- group specified in the @EC2SecurityGroupName@ field.
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the EC2 security group. Status can be
    -- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2SecurityGroupId', 'eC2SecurityGroup_eC2SecurityGroupId' - Specifies the id of the EC2 security group.
--
-- 'eC2SecurityGroupName', 'eC2SecurityGroup_eC2SecurityGroupName' - Specifies the name of the EC2 security group.
--
-- 'eC2SecurityGroupOwnerId', 'eC2SecurityGroup_eC2SecurityGroupOwnerId' - Specifies the Amazon Web Services ID of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ field.
--
-- 'status', 'eC2SecurityGroup_status' - Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
newEC2SecurityGroup ::
  EC2SecurityGroup
newEC2SecurityGroup =
  EC2SecurityGroup'
    { eC2SecurityGroupId =
        Prelude.Nothing,
      eC2SecurityGroupName = Prelude.Nothing,
      eC2SecurityGroupOwnerId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the id of the EC2 security group.
eC2SecurityGroup_eC2SecurityGroupId :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupId = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupId} -> eC2SecurityGroupId) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupId = a} :: EC2SecurityGroup)

-- | Specifies the name of the EC2 security group.
eC2SecurityGroup_eC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupName = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupName = a} :: EC2SecurityGroup)

-- | Specifies the Amazon Web Services ID of the owner of the EC2 security
-- group specified in the @EC2SecurityGroupName@ field.
eC2SecurityGroup_eC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupOwnerId = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupOwnerId = a} :: EC2SecurityGroup)

-- | Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
eC2SecurityGroup_status :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_status = Lens.lens (\EC2SecurityGroup' {status} -> status) (\s@EC2SecurityGroup' {} a -> s {status = a} :: EC2SecurityGroup)

instance Data.FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      Prelude.<$> (x Data..@? "EC2SecurityGroupId")
      Prelude.<*> (x Data..@? "EC2SecurityGroupName")
      Prelude.<*> (x Data..@? "EC2SecurityGroupOwnerId")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable EC2SecurityGroup where
  hashWithSalt _salt EC2SecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` eC2SecurityGroupId
      `Prelude.hashWithSalt` eC2SecurityGroupName
      `Prelude.hashWithSalt` eC2SecurityGroupOwnerId
      `Prelude.hashWithSalt` status

instance Prelude.NFData EC2SecurityGroup where
  rnf EC2SecurityGroup' {..} =
    Prelude.rnf eC2SecurityGroupId
      `Prelude.seq` Prelude.rnf eC2SecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf status
