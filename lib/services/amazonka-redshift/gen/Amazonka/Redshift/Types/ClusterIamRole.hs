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
-- Module      : Amazonka.Redshift.Types.ClusterIamRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterIamRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | An Identity and Access Management (IAM) role that can be used by the
-- associated Amazon Redshift cluster to access other Amazon Web Services
-- services.
--
-- /See:/ 'newClusterIamRole' smart constructor.
data ClusterIamRole = ClusterIamRole'
  { -- | A value that describes the status of the IAM role\'s association with an
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
    applyStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role, for example,
    -- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
    iamRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterIamRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'iamRoleArn', 'clusterIamRole_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role, for example,
-- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
newClusterIamRole ::
  ClusterIamRole
newClusterIamRole =
  ClusterIamRole'
    { applyStatus = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing
    }

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

-- | The Amazon Resource Name (ARN) of the IAM role, for example,
-- @arn:aws:iam::123456789012:role\/RedshiftCopyUnload@.
clusterIamRole_iamRoleArn :: Lens.Lens' ClusterIamRole (Prelude.Maybe Prelude.Text)
clusterIamRole_iamRoleArn = Lens.lens (\ClusterIamRole' {iamRoleArn} -> iamRoleArn) (\s@ClusterIamRole' {} a -> s {iamRoleArn = a} :: ClusterIamRole)

instance Data.FromXML ClusterIamRole where
  parseXML x =
    ClusterIamRole'
      Prelude.<$> (x Data..@? "ApplyStatus")
      Prelude.<*> (x Data..@? "IamRoleArn")

instance Prelude.Hashable ClusterIamRole where
  hashWithSalt _salt ClusterIamRole' {..} =
    _salt
      `Prelude.hashWithSalt` applyStatus
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData ClusterIamRole where
  rnf ClusterIamRole' {..} =
    Prelude.rnf applyStatus
      `Prelude.seq` Prelude.rnf iamRoleArn
