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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An IAM role that the cluster can use to access other Amazon Web Services
-- services.
--
-- /See:/ 'newAwsRedshiftClusterIamRole' smart constructor.
data AwsRedshiftClusterIamRole = AwsRedshiftClusterIamRole'
  { -- | The status of the IAM role\'s association with the cluster.
    --
    -- Valid values: @in-sync@ | @adding@ | @removing@
    applyStatus :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role.
    iamRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterIamRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyStatus', 'awsRedshiftClusterIamRole_applyStatus' - The status of the IAM role\'s association with the cluster.
--
-- Valid values: @in-sync@ | @adding@ | @removing@
--
-- 'iamRoleArn', 'awsRedshiftClusterIamRole_iamRoleArn' - The ARN of the IAM role.
newAwsRedshiftClusterIamRole ::
  AwsRedshiftClusterIamRole
newAwsRedshiftClusterIamRole =
  AwsRedshiftClusterIamRole'
    { applyStatus =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing
    }

-- | The status of the IAM role\'s association with the cluster.
--
-- Valid values: @in-sync@ | @adding@ | @removing@
awsRedshiftClusterIamRole_applyStatus :: Lens.Lens' AwsRedshiftClusterIamRole (Prelude.Maybe Prelude.Text)
awsRedshiftClusterIamRole_applyStatus = Lens.lens (\AwsRedshiftClusterIamRole' {applyStatus} -> applyStatus) (\s@AwsRedshiftClusterIamRole' {} a -> s {applyStatus = a} :: AwsRedshiftClusterIamRole)

-- | The ARN of the IAM role.
awsRedshiftClusterIamRole_iamRoleArn :: Lens.Lens' AwsRedshiftClusterIamRole (Prelude.Maybe Prelude.Text)
awsRedshiftClusterIamRole_iamRoleArn = Lens.lens (\AwsRedshiftClusterIamRole' {iamRoleArn} -> iamRoleArn) (\s@AwsRedshiftClusterIamRole' {} a -> s {iamRoleArn = a} :: AwsRedshiftClusterIamRole)

instance Data.FromJSON AwsRedshiftClusterIamRole where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterIamRole"
      ( \x ->
          AwsRedshiftClusterIamRole'
            Prelude.<$> (x Data..:? "ApplyStatus")
            Prelude.<*> (x Data..:? "IamRoleArn")
      )

instance Prelude.Hashable AwsRedshiftClusterIamRole where
  hashWithSalt _salt AwsRedshiftClusterIamRole' {..} =
    _salt `Prelude.hashWithSalt` applyStatus
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData AwsRedshiftClusterIamRole where
  rnf AwsRedshiftClusterIamRole' {..} =
    Prelude.rnf applyStatus
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Data.ToJSON AwsRedshiftClusterIamRole where
  toJSON AwsRedshiftClusterIamRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplyStatus" Data..=) Prelude.<$> applyStatus,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn
          ]
      )
