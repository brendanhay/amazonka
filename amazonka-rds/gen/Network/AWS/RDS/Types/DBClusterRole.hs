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
-- Module      : Network.AWS.RDS.Types.DBClusterRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterRole where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an AWS Identity and Access Management (IAM) role that is
-- associated with a DB cluster.
--
-- /See:/ 'newDBClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { -- | Describes the state of association between the IAM role and the DB
    -- cluster. The Status property returns one of the following values:
    --
    -- -   @ACTIVE@ - the IAM role ARN is associated with the DB cluster and
    --     can be used to access other AWS services on your behalf.
    --
    -- -   @PENDING@ - the IAM role ARN is being associated with the DB
    --     cluster.
    --
    -- -   @INVALID@ - the IAM role ARN is associated with the DB cluster, but
    --     the DB cluster is unable to assume the IAM role in order to access
    --     other AWS services on your behalf.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that is associated with
    -- the DB cluster.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature associated with the AWS Identity and Access
    -- Management (IAM) role. For the list of supported feature names, see
    -- DBEngineVersion.
    featureName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBClusterRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbClusterRole_status' - Describes the state of association between the IAM role and the DB
-- cluster. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB cluster and
--     can be used to access other AWS services on your behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     cluster.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB cluster, but
--     the DB cluster is unable to assume the IAM role in order to access
--     other AWS services on your behalf.
--
-- 'roleArn', 'dbClusterRole_roleArn' - The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB cluster.
--
-- 'featureName', 'dbClusterRole_featureName' - The name of the feature associated with the AWS Identity and Access
-- Management (IAM) role. For the list of supported feature names, see
-- DBEngineVersion.
newDBClusterRole ::
  DBClusterRole
newDBClusterRole =
  DBClusterRole'
    { status = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      featureName = Prelude.Nothing
    }

-- | Describes the state of association between the IAM role and the DB
-- cluster. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB cluster and
--     can be used to access other AWS services on your behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     cluster.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB cluster, but
--     the DB cluster is unable to assume the IAM role in order to access
--     other AWS services on your behalf.
dbClusterRole_status :: Lens.Lens' DBClusterRole (Prelude.Maybe Prelude.Text)
dbClusterRole_status = Lens.lens (\DBClusterRole' {status} -> status) (\s@DBClusterRole' {} a -> s {status = a} :: DBClusterRole)

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB cluster.
dbClusterRole_roleArn :: Lens.Lens' DBClusterRole (Prelude.Maybe Prelude.Text)
dbClusterRole_roleArn = Lens.lens (\DBClusterRole' {roleArn} -> roleArn) (\s@DBClusterRole' {} a -> s {roleArn = a} :: DBClusterRole)

-- | The name of the feature associated with the AWS Identity and Access
-- Management (IAM) role. For the list of supported feature names, see
-- DBEngineVersion.
dbClusterRole_featureName :: Lens.Lens' DBClusterRole (Prelude.Maybe Prelude.Text)
dbClusterRole_featureName = Lens.lens (\DBClusterRole' {featureName} -> featureName) (\s@DBClusterRole' {} a -> s {featureName = a} :: DBClusterRole)

instance Core.FromXML DBClusterRole where
  parseXML x =
    DBClusterRole'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "RoleArn")
      Prelude.<*> (x Core..@? "FeatureName")

instance Prelude.Hashable DBClusterRole

instance Prelude.NFData DBClusterRole
