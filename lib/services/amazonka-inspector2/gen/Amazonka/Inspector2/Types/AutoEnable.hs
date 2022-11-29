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
-- Module      : Amazonka.Inspector2.Types.AutoEnable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AutoEnable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents which scan types are automatically enabled for new members of
-- your Amazon Inspector organization.
--
-- /See:/ 'newAutoEnable' smart constructor.
data AutoEnable = AutoEnable'
  { -- | Represents whether Amazon EC2 scans are automatically enabled for new
    -- members of your Amazon Inspector organization.
    ec2 :: Prelude.Bool,
    -- | Represents whether Amazon ECR scans are automatically enabled for new
    -- members of your Amazon Inspector organization.
    ecr :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoEnable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2', 'autoEnable_ec2' - Represents whether Amazon EC2 scans are automatically enabled for new
-- members of your Amazon Inspector organization.
--
-- 'ecr', 'autoEnable_ecr' - Represents whether Amazon ECR scans are automatically enabled for new
-- members of your Amazon Inspector organization.
newAutoEnable ::
  -- | 'ec2'
  Prelude.Bool ->
  -- | 'ecr'
  Prelude.Bool ->
  AutoEnable
newAutoEnable pEc2_ pEcr_ =
  AutoEnable' {ec2 = pEc2_, ecr = pEcr_}

-- | Represents whether Amazon EC2 scans are automatically enabled for new
-- members of your Amazon Inspector organization.
autoEnable_ec2 :: Lens.Lens' AutoEnable Prelude.Bool
autoEnable_ec2 = Lens.lens (\AutoEnable' {ec2} -> ec2) (\s@AutoEnable' {} a -> s {ec2 = a} :: AutoEnable)

-- | Represents whether Amazon ECR scans are automatically enabled for new
-- members of your Amazon Inspector organization.
autoEnable_ecr :: Lens.Lens' AutoEnable Prelude.Bool
autoEnable_ecr = Lens.lens (\AutoEnable' {ecr} -> ecr) (\s@AutoEnable' {} a -> s {ecr = a} :: AutoEnable)

instance Core.FromJSON AutoEnable where
  parseJSON =
    Core.withObject
      "AutoEnable"
      ( \x ->
          AutoEnable'
            Prelude.<$> (x Core..: "ec2") Prelude.<*> (x Core..: "ecr")
      )

instance Prelude.Hashable AutoEnable where
  hashWithSalt _salt AutoEnable' {..} =
    _salt `Prelude.hashWithSalt` ec2
      `Prelude.hashWithSalt` ecr

instance Prelude.NFData AutoEnable where
  rnf AutoEnable' {..} =
    Prelude.rnf ec2 `Prelude.seq` Prelude.rnf ecr

instance Core.ToJSON AutoEnable where
  toJSON AutoEnable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ec2" Core..= ec2),
            Prelude.Just ("ecr" Core..= ecr)
          ]
      )
