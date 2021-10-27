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
-- Module      : Network.AWS.SecurityHub.Types.AwsRdsDbInstanceAssociatedRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRdsDbInstanceAssociatedRole where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An IAM role associated with the DB instance.
--
-- /See:/ 'newAwsRdsDbInstanceAssociatedRole' smart constructor.
data AwsRdsDbInstanceAssociatedRole = AwsRdsDbInstanceAssociatedRole'
  { -- | Describes the state of the association between the IAM role and the DB
    -- instance. The @Status@ property returns one of the following values:
    --
    -- -   @ACTIVE@ - The IAM role ARN is associated with the DB instance and
    --     can be used to access other Amazon Web Services services on your
    --     behalf.
    --
    -- -   @PENDING@ - The IAM role ARN is being associated with the DB
    --     instance.
    --
    -- -   @INVALID@ - The IAM role ARN is associated with the DB instance. But
    --     the DB instance is unable to assume the IAM role in order to access
    --     other Amazon Web Services services on your behalf.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature associated with the IAM role.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that is associated with the DB instance.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbInstanceAssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbInstanceAssociatedRole_status' - Describes the state of the association between the IAM role and the DB
-- instance. The @Status@ property returns one of the following values:
--
-- -   @ACTIVE@ - The IAM role ARN is associated with the DB instance and
--     can be used to access other Amazon Web Services services on your
--     behalf.
--
-- -   @PENDING@ - The IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - The IAM role ARN is associated with the DB instance. But
--     the DB instance is unable to assume the IAM role in order to access
--     other Amazon Web Services services on your behalf.
--
-- 'featureName', 'awsRdsDbInstanceAssociatedRole_featureName' - The name of the feature associated with the IAM role.
--
-- 'roleArn', 'awsRdsDbInstanceAssociatedRole_roleArn' - The ARN of the IAM role that is associated with the DB instance.
newAwsRdsDbInstanceAssociatedRole ::
  AwsRdsDbInstanceAssociatedRole
newAwsRdsDbInstanceAssociatedRole =
  AwsRdsDbInstanceAssociatedRole'
    { status =
        Prelude.Nothing,
      featureName = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | Describes the state of the association between the IAM role and the DB
-- instance. The @Status@ property returns one of the following values:
--
-- -   @ACTIVE@ - The IAM role ARN is associated with the DB instance and
--     can be used to access other Amazon Web Services services on your
--     behalf.
--
-- -   @PENDING@ - The IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - The IAM role ARN is associated with the DB instance. But
--     the DB instance is unable to assume the IAM role in order to access
--     other Amazon Web Services services on your behalf.
awsRdsDbInstanceAssociatedRole_status :: Lens.Lens' AwsRdsDbInstanceAssociatedRole (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceAssociatedRole_status = Lens.lens (\AwsRdsDbInstanceAssociatedRole' {status} -> status) (\s@AwsRdsDbInstanceAssociatedRole' {} a -> s {status = a} :: AwsRdsDbInstanceAssociatedRole)

-- | The name of the feature associated with the IAM role.
awsRdsDbInstanceAssociatedRole_featureName :: Lens.Lens' AwsRdsDbInstanceAssociatedRole (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceAssociatedRole_featureName = Lens.lens (\AwsRdsDbInstanceAssociatedRole' {featureName} -> featureName) (\s@AwsRdsDbInstanceAssociatedRole' {} a -> s {featureName = a} :: AwsRdsDbInstanceAssociatedRole)

-- | The ARN of the IAM role that is associated with the DB instance.
awsRdsDbInstanceAssociatedRole_roleArn :: Lens.Lens' AwsRdsDbInstanceAssociatedRole (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceAssociatedRole_roleArn = Lens.lens (\AwsRdsDbInstanceAssociatedRole' {roleArn} -> roleArn) (\s@AwsRdsDbInstanceAssociatedRole' {} a -> s {roleArn = a} :: AwsRdsDbInstanceAssociatedRole)

instance Core.FromJSON AwsRdsDbInstanceAssociatedRole where
  parseJSON =
    Core.withObject
      "AwsRdsDbInstanceAssociatedRole"
      ( \x ->
          AwsRdsDbInstanceAssociatedRole'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FeatureName")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance
  Prelude.Hashable
    AwsRdsDbInstanceAssociatedRole

instance
  Prelude.NFData
    AwsRdsDbInstanceAssociatedRole

instance Core.ToJSON AwsRdsDbInstanceAssociatedRole where
  toJSON AwsRdsDbInstanceAssociatedRole' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("FeatureName" Core..=) Prelude.<$> featureName,
            ("RoleArn" Core..=) Prelude.<$> roleArn
          ]
      )
