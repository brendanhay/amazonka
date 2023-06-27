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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AutoEnable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents which scan types are automatically enabled for new members of
-- your Amazon Inspector organization.
--
-- /See:/ 'newAutoEnable' smart constructor.
data AutoEnable = AutoEnable'
  { -- | Represents whether AWS Lambda standard scans are automatically enabled
    -- for new members of your Amazon Inspector organization.
    lambda :: Prelude.Maybe Prelude.Bool,
    -- | Represents whether AWS Lambda code scans are automatically enabled for
    -- new members of your Amazon Inspector organization.
    --
    -- >  </p>
    lambdaCode :: Prelude.Maybe Prelude.Bool,
    -- | Represents whether Amazon EC2 scans are automatically enabled for new
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
-- 'lambda', 'autoEnable_lambda' - Represents whether AWS Lambda standard scans are automatically enabled
-- for new members of your Amazon Inspector organization.
--
-- 'lambdaCode', 'autoEnable_lambdaCode' - Represents whether AWS Lambda code scans are automatically enabled for
-- new members of your Amazon Inspector organization.
--
-- >  </p>
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
  AutoEnable'
    { lambda = Prelude.Nothing,
      lambdaCode = Prelude.Nothing,
      ec2 = pEc2_,
      ecr = pEcr_
    }

-- | Represents whether AWS Lambda standard scans are automatically enabled
-- for new members of your Amazon Inspector organization.
autoEnable_lambda :: Lens.Lens' AutoEnable (Prelude.Maybe Prelude.Bool)
autoEnable_lambda = Lens.lens (\AutoEnable' {lambda} -> lambda) (\s@AutoEnable' {} a -> s {lambda = a} :: AutoEnable)

-- | Represents whether AWS Lambda code scans are automatically enabled for
-- new members of your Amazon Inspector organization.
--
-- >  </p>
autoEnable_lambdaCode :: Lens.Lens' AutoEnable (Prelude.Maybe Prelude.Bool)
autoEnable_lambdaCode = Lens.lens (\AutoEnable' {lambdaCode} -> lambdaCode) (\s@AutoEnable' {} a -> s {lambdaCode = a} :: AutoEnable)

-- | Represents whether Amazon EC2 scans are automatically enabled for new
-- members of your Amazon Inspector organization.
autoEnable_ec2 :: Lens.Lens' AutoEnable Prelude.Bool
autoEnable_ec2 = Lens.lens (\AutoEnable' {ec2} -> ec2) (\s@AutoEnable' {} a -> s {ec2 = a} :: AutoEnable)

-- | Represents whether Amazon ECR scans are automatically enabled for new
-- members of your Amazon Inspector organization.
autoEnable_ecr :: Lens.Lens' AutoEnable Prelude.Bool
autoEnable_ecr = Lens.lens (\AutoEnable' {ecr} -> ecr) (\s@AutoEnable' {} a -> s {ecr = a} :: AutoEnable)

instance Data.FromJSON AutoEnable where
  parseJSON =
    Data.withObject
      "AutoEnable"
      ( \x ->
          AutoEnable'
            Prelude.<$> (x Data..:? "lambda")
            Prelude.<*> (x Data..:? "lambdaCode")
            Prelude.<*> (x Data..: "ec2")
            Prelude.<*> (x Data..: "ecr")
      )

instance Prelude.Hashable AutoEnable where
  hashWithSalt _salt AutoEnable' {..} =
    _salt
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` lambdaCode
      `Prelude.hashWithSalt` ec2
      `Prelude.hashWithSalt` ecr

instance Prelude.NFData AutoEnable where
  rnf AutoEnable' {..} =
    Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf lambdaCode
      `Prelude.seq` Prelude.rnf ec2
      `Prelude.seq` Prelude.rnf ecr

instance Data.ToJSON AutoEnable where
  toJSON AutoEnable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lambda" Data..=) Prelude.<$> lambda,
            ("lambdaCode" Data..=) Prelude.<$> lambdaCode,
            Prelude.Just ("ec2" Data..= ec2),
            Prelude.Just ("ecr" Data..= ecr)
          ]
      )
