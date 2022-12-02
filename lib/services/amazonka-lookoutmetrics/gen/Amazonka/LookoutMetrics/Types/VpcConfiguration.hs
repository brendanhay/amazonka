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
-- Module      : Amazonka.LookoutMetrics.Types.VpcConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information about the Amazon Virtual Private
-- Cloud (VPC).
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | An array of strings containing the Amazon VPC subnet IDs (e.g.,
    -- @subnet-0bb1c79de3EXAMPLE@.
    subnetIdList :: [Prelude.Text],
    -- | An array of strings containing the list of security groups.
    securityGroupIdList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetIdList', 'vpcConfiguration_subnetIdList' - An array of strings containing the Amazon VPC subnet IDs (e.g.,
-- @subnet-0bb1c79de3EXAMPLE@.
--
-- 'securityGroupIdList', 'vpcConfiguration_securityGroupIdList' - An array of strings containing the list of security groups.
newVpcConfiguration ::
  VpcConfiguration
newVpcConfiguration =
  VpcConfiguration'
    { subnetIdList = Prelude.mempty,
      securityGroupIdList = Prelude.mempty
    }

-- | An array of strings containing the Amazon VPC subnet IDs (e.g.,
-- @subnet-0bb1c79de3EXAMPLE@.
vpcConfiguration_subnetIdList :: Lens.Lens' VpcConfiguration [Prelude.Text]
vpcConfiguration_subnetIdList = Lens.lens (\VpcConfiguration' {subnetIdList} -> subnetIdList) (\s@VpcConfiguration' {} a -> s {subnetIdList = a} :: VpcConfiguration) Prelude.. Lens.coerced

-- | An array of strings containing the list of security groups.
vpcConfiguration_securityGroupIdList :: Lens.Lens' VpcConfiguration [Prelude.Text]
vpcConfiguration_securityGroupIdList = Lens.lens (\VpcConfiguration' {securityGroupIdList} -> securityGroupIdList) (\s@VpcConfiguration' {} a -> s {securityGroupIdList = a} :: VpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfiguration where
  parseJSON =
    Data.withObject
      "VpcConfiguration"
      ( \x ->
          VpcConfiguration'
            Prelude.<$> (x Data..:? "SubnetIdList" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "SecurityGroupIdList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` subnetIdList
      `Prelude.hashWithSalt` securityGroupIdList

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf subnetIdList
      `Prelude.seq` Prelude.rnf securityGroupIdList

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SubnetIdList" Data..= subnetIdList),
            Prelude.Just
              ("SecurityGroupIdList" Data..= securityGroupIdList)
          ]
      )
