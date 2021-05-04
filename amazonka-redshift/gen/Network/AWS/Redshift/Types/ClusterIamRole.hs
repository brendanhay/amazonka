{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.ClusterIamRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterIamRole where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | An AWS Identity and Access Management (IAM) role that can be used by the
-- associated Amazon Redshift cluster to access other AWS services.
--
-- /See:/ 'newClusterIamRole' smart constructor.
data ClusterIamRole = ClusterIamRole'
  { -- | The Amazon Resource Name (ARN) of the IAM role, for example,
    -- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A value that describes the status of the IAM role\'s association with an
    -- Amazon Redshift cluster.
    --
    -- The following are possible statuses and descriptions.
    --
    -- -   @in-sync@: The role is available for use by the cluster.
    --
    -- -   @adding@: The role is in the process of being associated with the
    --     cluster.
    --
    -- -   @removing@: The role is in the process of being disassociated with
    --     the cluster.
    applyStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClusterIamRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamRoleArn', 'clusterIamRole_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role, for example,
-- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
--
-- 'applyStatus', 'clusterIamRole_applyStatus' - A value that describes the status of the IAM role\'s association with an
-- Amazon Redshift cluster.
--
-- The following are possible statuses and descriptions.
--
-- -   @in-sync@: The role is available for use by the cluster.
--
-- -   @adding@: The role is in the process of being associated with the
--     cluster.
--
-- -   @removing@: The role is in the process of being disassociated with
--     the cluster.
newClusterIamRole ::
  ClusterIamRole
newClusterIamRole =
  ClusterIamRole'
    { iamRoleArn = Prelude.Nothing,
      applyStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role, for example,
-- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
clusterIamRole_iamRoleArn :: Lens.Lens' ClusterIamRole (Prelude.Maybe Prelude.Text)
clusterIamRole_iamRoleArn = Lens.lens (\ClusterIamRole' {iamRoleArn} -> iamRoleArn) (\s@ClusterIamRole' {} a -> s {iamRoleArn = a} :: ClusterIamRole)

-- | A value that describes the status of the IAM role\'s association with an
-- Amazon Redshift cluster.
--
-- The following are possible statuses and descriptions.
--
-- -   @in-sync@: The role is available for use by the cluster.
--
-- -   @adding@: The role is in the process of being associated with the
--     cluster.
--
-- -   @removing@: The role is in the process of being disassociated with
--     the cluster.
clusterIamRole_applyStatus :: Lens.Lens' ClusterIamRole (Prelude.Maybe Prelude.Text)
clusterIamRole_applyStatus = Lens.lens (\ClusterIamRole' {applyStatus} -> applyStatus) (\s@ClusterIamRole' {} a -> s {applyStatus = a} :: ClusterIamRole)

instance Prelude.FromXML ClusterIamRole where
  parseXML x =
    ClusterIamRole'
      Prelude.<$> (x Prelude..@? "IamRoleArn")
      Prelude.<*> (x Prelude..@? "ApplyStatus")

instance Prelude.Hashable ClusterIamRole

instance Prelude.NFData ClusterIamRole
