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
-- Module      : Amazonka.AccessAnalyzer.Types.VpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed virtual private cloud (VPC) configuration for the Amazon S3
-- access point. VPC configuration does not apply to multi-region access
-- points. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_VpcConfiguration.html VpcConfiguration>.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | If this field is specified, this access point will only allow
    -- connections from the specified VPC ID.
    vpcId :: Prelude.Text
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
-- 'vpcId', 'vpcConfiguration_vpcId' - If this field is specified, this access point will only allow
-- connections from the specified VPC ID.
newVpcConfiguration ::
  -- | 'vpcId'
  Prelude.Text ->
  VpcConfiguration
newVpcConfiguration pVpcId_ =
  VpcConfiguration' {vpcId = pVpcId_}

-- | If this field is specified, this access point will only allow
-- connections from the specified VPC ID.
vpcConfiguration_vpcId :: Lens.Lens' VpcConfiguration Prelude.Text
vpcConfiguration_vpcId = Lens.lens (\VpcConfiguration' {vpcId} -> vpcId) (\s@VpcConfiguration' {} a -> s {vpcId = a} :: VpcConfiguration)

instance Data.FromJSON VpcConfiguration where
  parseJSON =
    Data.withObject
      "VpcConfiguration"
      ( \x ->
          VpcConfiguration' Prelude.<$> (x Data..: "vpcId")
      )

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} = Prelude.rnf vpcId

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("vpcId" Data..= vpcId)]
      )
