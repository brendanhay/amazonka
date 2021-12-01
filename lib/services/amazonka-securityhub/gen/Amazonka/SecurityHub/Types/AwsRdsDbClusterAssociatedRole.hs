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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbClusterAssociatedRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbClusterAssociatedRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An IAM role that is associated with the Amazon RDS DB cluster.
--
-- /See:/ 'newAwsRdsDbClusterAssociatedRole' smart constructor.
data AwsRdsDbClusterAssociatedRole = AwsRdsDbClusterAssociatedRole'
  { -- | The status of the association between the IAM role and the DB cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbClusterAssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbClusterAssociatedRole_status' - The status of the association between the IAM role and the DB cluster.
--
-- 'roleArn', 'awsRdsDbClusterAssociatedRole_roleArn' - The ARN of the IAM role.
newAwsRdsDbClusterAssociatedRole ::
  AwsRdsDbClusterAssociatedRole
newAwsRdsDbClusterAssociatedRole =
  AwsRdsDbClusterAssociatedRole'
    { status =
        Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The status of the association between the IAM role and the DB cluster.
awsRdsDbClusterAssociatedRole_status :: Lens.Lens' AwsRdsDbClusterAssociatedRole (Prelude.Maybe Prelude.Text)
awsRdsDbClusterAssociatedRole_status = Lens.lens (\AwsRdsDbClusterAssociatedRole' {status} -> status) (\s@AwsRdsDbClusterAssociatedRole' {} a -> s {status = a} :: AwsRdsDbClusterAssociatedRole)

-- | The ARN of the IAM role.
awsRdsDbClusterAssociatedRole_roleArn :: Lens.Lens' AwsRdsDbClusterAssociatedRole (Prelude.Maybe Prelude.Text)
awsRdsDbClusterAssociatedRole_roleArn = Lens.lens (\AwsRdsDbClusterAssociatedRole' {roleArn} -> roleArn) (\s@AwsRdsDbClusterAssociatedRole' {} a -> s {roleArn = a} :: AwsRdsDbClusterAssociatedRole)

instance Core.FromJSON AwsRdsDbClusterAssociatedRole where
  parseJSON =
    Core.withObject
      "AwsRdsDbClusterAssociatedRole"
      ( \x ->
          AwsRdsDbClusterAssociatedRole'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance
  Prelude.Hashable
    AwsRdsDbClusterAssociatedRole
  where
  hashWithSalt salt' AwsRdsDbClusterAssociatedRole' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsRdsDbClusterAssociatedRole where
  rnf AwsRdsDbClusterAssociatedRole' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToJSON AwsRdsDbClusterAssociatedRole where
  toJSON AwsRdsDbClusterAssociatedRole' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("RoleArn" Core..=) Prelude.<$> roleArn
          ]
      )
