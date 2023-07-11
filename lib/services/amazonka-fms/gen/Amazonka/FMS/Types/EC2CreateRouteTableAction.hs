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
-- Module      : Amazonka.FMS.Types.EC2CreateRouteTableAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2CreateRouteTableAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Prelude as Prelude

-- | Information about the CreateRouteTable action in Amazon EC2.
--
-- /See:/ 'newEC2CreateRouteTableAction' smart constructor.
data EC2CreateRouteTableAction = EC2CreateRouteTableAction'
  { -- | A description of the CreateRouteTable action.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of a VPC.
    vpcId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2CreateRouteTableAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'eC2CreateRouteTableAction_description' - A description of the CreateRouteTable action.
--
-- 'vpcId', 'eC2CreateRouteTableAction_vpcId' - Information about the ID of a VPC.
newEC2CreateRouteTableAction ::
  -- | 'vpcId'
  ActionTarget ->
  EC2CreateRouteTableAction
newEC2CreateRouteTableAction pVpcId_ =
  EC2CreateRouteTableAction'
    { description =
        Prelude.Nothing,
      vpcId = pVpcId_
    }

-- | A description of the CreateRouteTable action.
eC2CreateRouteTableAction_description :: Lens.Lens' EC2CreateRouteTableAction (Prelude.Maybe Prelude.Text)
eC2CreateRouteTableAction_description = Lens.lens (\EC2CreateRouteTableAction' {description} -> description) (\s@EC2CreateRouteTableAction' {} a -> s {description = a} :: EC2CreateRouteTableAction)

-- | Information about the ID of a VPC.
eC2CreateRouteTableAction_vpcId :: Lens.Lens' EC2CreateRouteTableAction ActionTarget
eC2CreateRouteTableAction_vpcId = Lens.lens (\EC2CreateRouteTableAction' {vpcId} -> vpcId) (\s@EC2CreateRouteTableAction' {} a -> s {vpcId = a} :: EC2CreateRouteTableAction)

instance Data.FromJSON EC2CreateRouteTableAction where
  parseJSON =
    Data.withObject
      "EC2CreateRouteTableAction"
      ( \x ->
          EC2CreateRouteTableAction'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "VpcId")
      )

instance Prelude.Hashable EC2CreateRouteTableAction where
  hashWithSalt _salt EC2CreateRouteTableAction' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData EC2CreateRouteTableAction where
  rnf EC2CreateRouteTableAction' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf vpcId
