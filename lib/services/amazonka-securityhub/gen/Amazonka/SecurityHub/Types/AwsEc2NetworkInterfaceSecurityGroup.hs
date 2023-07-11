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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A security group associated with the network interface.
--
-- /See:/ 'newAwsEc2NetworkInterfaceSecurityGroup' smart constructor.
data AwsEc2NetworkInterfaceSecurityGroup = AwsEc2NetworkInterfaceSecurityGroup'
  { -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'awsEc2NetworkInterfaceSecurityGroup_groupId' - The ID of the security group.
--
-- 'groupName', 'awsEc2NetworkInterfaceSecurityGroup_groupName' - The name of the security group.
newAwsEc2NetworkInterfaceSecurityGroup ::
  AwsEc2NetworkInterfaceSecurityGroup
newAwsEc2NetworkInterfaceSecurityGroup =
  AwsEc2NetworkInterfaceSecurityGroup'
    { groupId =
        Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The ID of the security group.
awsEc2NetworkInterfaceSecurityGroup_groupId :: Lens.Lens' AwsEc2NetworkInterfaceSecurityGroup (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceSecurityGroup_groupId = Lens.lens (\AwsEc2NetworkInterfaceSecurityGroup' {groupId} -> groupId) (\s@AwsEc2NetworkInterfaceSecurityGroup' {} a -> s {groupId = a} :: AwsEc2NetworkInterfaceSecurityGroup)

-- | The name of the security group.
awsEc2NetworkInterfaceSecurityGroup_groupName :: Lens.Lens' AwsEc2NetworkInterfaceSecurityGroup (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceSecurityGroup_groupName = Lens.lens (\AwsEc2NetworkInterfaceSecurityGroup' {groupName} -> groupName) (\s@AwsEc2NetworkInterfaceSecurityGroup' {} a -> s {groupName = a} :: AwsEc2NetworkInterfaceSecurityGroup)

instance
  Data.FromJSON
    AwsEc2NetworkInterfaceSecurityGroup
  where
  parseJSON =
    Data.withObject
      "AwsEc2NetworkInterfaceSecurityGroup"
      ( \x ->
          AwsEc2NetworkInterfaceSecurityGroup'
            Prelude.<$> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "GroupName")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceSecurityGroup
  where
  hashWithSalt
    _salt
    AwsEc2NetworkInterfaceSecurityGroup' {..} =
      _salt
        `Prelude.hashWithSalt` groupId
        `Prelude.hashWithSalt` groupName

instance
  Prelude.NFData
    AwsEc2NetworkInterfaceSecurityGroup
  where
  rnf AwsEc2NetworkInterfaceSecurityGroup' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName

instance
  Data.ToJSON
    AwsEc2NetworkInterfaceSecurityGroup
  where
  toJSON AwsEc2NetworkInterfaceSecurityGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupId" Data..=) Prelude.<$> groupId,
            ("GroupName" Data..=) Prelude.<$> groupName
          ]
      )
