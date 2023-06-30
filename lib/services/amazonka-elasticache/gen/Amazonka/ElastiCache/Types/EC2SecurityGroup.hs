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
-- Module      : Amazonka.ElastiCache.Types.EC2SecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.EC2SecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides ownership and status information for an Amazon EC2 security
-- group.
--
-- /See:/ 'newEC2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { -- | The name of the Amazon EC2 security group.
    eC2SecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon account ID of the Amazon EC2 security group owner.
    eC2SecurityGroupOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon EC2 security group.
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
-- 'eC2SecurityGroupName', 'eC2SecurityGroup_eC2SecurityGroupName' - The name of the Amazon EC2 security group.
--
-- 'eC2SecurityGroupOwnerId', 'eC2SecurityGroup_eC2SecurityGroupOwnerId' - The Amazon account ID of the Amazon EC2 security group owner.
--
-- 'status', 'eC2SecurityGroup_status' - The status of the Amazon EC2 security group.
newEC2SecurityGroup ::
  EC2SecurityGroup
newEC2SecurityGroup =
  EC2SecurityGroup'
    { eC2SecurityGroupName =
        Prelude.Nothing,
      eC2SecurityGroupOwnerId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the Amazon EC2 security group.
eC2SecurityGroup_eC2SecurityGroupName :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupName = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupName} -> eC2SecurityGroupName) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupName = a} :: EC2SecurityGroup)

-- | The Amazon account ID of the Amazon EC2 security group owner.
eC2SecurityGroup_eC2SecurityGroupOwnerId :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_eC2SecurityGroupOwnerId = Lens.lens (\EC2SecurityGroup' {eC2SecurityGroupOwnerId} -> eC2SecurityGroupOwnerId) (\s@EC2SecurityGroup' {} a -> s {eC2SecurityGroupOwnerId = a} :: EC2SecurityGroup)

-- | The status of the Amazon EC2 security group.
eC2SecurityGroup_status :: Lens.Lens' EC2SecurityGroup (Prelude.Maybe Prelude.Text)
eC2SecurityGroup_status = Lens.lens (\EC2SecurityGroup' {status} -> status) (\s@EC2SecurityGroup' {} a -> s {status = a} :: EC2SecurityGroup)

instance Data.FromXML EC2SecurityGroup where
  parseXML x =
    EC2SecurityGroup'
      Prelude.<$> (x Data..@? "EC2SecurityGroupName")
      Prelude.<*> (x Data..@? "EC2SecurityGroupOwnerId")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable EC2SecurityGroup where
  hashWithSalt _salt EC2SecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` eC2SecurityGroupName
      `Prelude.hashWithSalt` eC2SecurityGroupOwnerId
      `Prelude.hashWithSalt` status

instance Prelude.NFData EC2SecurityGroup where
  rnf EC2SecurityGroup' {..} =
    Prelude.rnf eC2SecurityGroupName
      `Prelude.seq` Prelude.rnf eC2SecurityGroupOwnerId
      `Prelude.seq` Prelude.rnf status
