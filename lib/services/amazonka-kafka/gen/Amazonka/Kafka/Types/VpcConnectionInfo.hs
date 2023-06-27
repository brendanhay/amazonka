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
-- Module      : Amazonka.Kafka.Types.VpcConnectionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.UserIdentity
import qualified Amazonka.Prelude as Prelude

-- | Description of the VPC connection.
--
-- /See:/ 'newVpcConnectionInfo' smart constructor.
data VpcConnectionInfo = VpcConnectionInfo'
  { -- | The time when Amazon MSK creates the VPC Connnection.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The owner of the VPC Connection.
    owner :: Prelude.Maybe Prelude.Text,
    -- | Description of the requester that calls the API operation.
    userIdentity :: Prelude.Maybe UserIdentity,
    -- | The Amazon Resource Name (ARN) of the VPC connection.
    vpcConnectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'vpcConnectionInfo_creationTime' - The time when Amazon MSK creates the VPC Connnection.
--
-- 'owner', 'vpcConnectionInfo_owner' - The owner of the VPC Connection.
--
-- 'userIdentity', 'vpcConnectionInfo_userIdentity' - Description of the requester that calls the API operation.
--
-- 'vpcConnectionArn', 'vpcConnectionInfo_vpcConnectionArn' - The Amazon Resource Name (ARN) of the VPC connection.
newVpcConnectionInfo ::
  VpcConnectionInfo
newVpcConnectionInfo =
  VpcConnectionInfo'
    { creationTime = Prelude.Nothing,
      owner = Prelude.Nothing,
      userIdentity = Prelude.Nothing,
      vpcConnectionArn = Prelude.Nothing
    }

-- | The time when Amazon MSK creates the VPC Connnection.
vpcConnectionInfo_creationTime :: Lens.Lens' VpcConnectionInfo (Prelude.Maybe Prelude.UTCTime)
vpcConnectionInfo_creationTime = Lens.lens (\VpcConnectionInfo' {creationTime} -> creationTime) (\s@VpcConnectionInfo' {} a -> s {creationTime = a} :: VpcConnectionInfo) Prelude.. Lens.mapping Data._Time

-- | The owner of the VPC Connection.
vpcConnectionInfo_owner :: Lens.Lens' VpcConnectionInfo (Prelude.Maybe Prelude.Text)
vpcConnectionInfo_owner = Lens.lens (\VpcConnectionInfo' {owner} -> owner) (\s@VpcConnectionInfo' {} a -> s {owner = a} :: VpcConnectionInfo)

-- | Description of the requester that calls the API operation.
vpcConnectionInfo_userIdentity :: Lens.Lens' VpcConnectionInfo (Prelude.Maybe UserIdentity)
vpcConnectionInfo_userIdentity = Lens.lens (\VpcConnectionInfo' {userIdentity} -> userIdentity) (\s@VpcConnectionInfo' {} a -> s {userIdentity = a} :: VpcConnectionInfo)

-- | The Amazon Resource Name (ARN) of the VPC connection.
vpcConnectionInfo_vpcConnectionArn :: Lens.Lens' VpcConnectionInfo (Prelude.Maybe Prelude.Text)
vpcConnectionInfo_vpcConnectionArn = Lens.lens (\VpcConnectionInfo' {vpcConnectionArn} -> vpcConnectionArn) (\s@VpcConnectionInfo' {} a -> s {vpcConnectionArn = a} :: VpcConnectionInfo)

instance Data.FromJSON VpcConnectionInfo where
  parseJSON =
    Data.withObject
      "VpcConnectionInfo"
      ( \x ->
          VpcConnectionInfo'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "userIdentity")
            Prelude.<*> (x Data..:? "vpcConnectionArn")
      )

instance Prelude.Hashable VpcConnectionInfo where
  hashWithSalt _salt VpcConnectionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` userIdentity
      `Prelude.hashWithSalt` vpcConnectionArn

instance Prelude.NFData VpcConnectionInfo where
  rnf VpcConnectionInfo' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf userIdentity
      `Prelude.seq` Prelude.rnf vpcConnectionArn
