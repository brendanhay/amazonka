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
-- Module      : Network.AWS.RDS.Types.DBInstanceRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceRole where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an AWS Identity and Access Management (IAM) role that is
-- associated with a DB instance.
--
-- /See:/ 'newDBInstanceRole' smart constructor.
data DBInstanceRole = DBInstanceRole'
  { -- | Describes the state of association between the IAM role and the DB
    -- instance. The Status property returns one of the following values:
    --
    -- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
    --     can be used to access other AWS services on your behalf.
    --
    -- -   @PENDING@ - the IAM role ARN is being associated with the DB
    --     instance.
    --
    -- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
    --     the DB instance is unable to assume the IAM role in order to access
    --     other AWS services on your behalf.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that is associated with
    -- the DB instance.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature associated with the AWS Identity and Access
    -- Management (IAM) role. For the list of supported feature names, see
    -- @DBEngineVersion@.
    featureName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBInstanceRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dbInstanceRole_status' - Describes the state of association between the IAM role and the DB
-- instance. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
--     can be used to access other AWS services on your behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
--     the DB instance is unable to assume the IAM role in order to access
--     other AWS services on your behalf.
--
-- 'roleArn', 'dbInstanceRole_roleArn' - The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB instance.
--
-- 'featureName', 'dbInstanceRole_featureName' - The name of the feature associated with the AWS Identity and Access
-- Management (IAM) role. For the list of supported feature names, see
-- @DBEngineVersion@.
newDBInstanceRole ::
  DBInstanceRole
newDBInstanceRole =
  DBInstanceRole'
    { status = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      featureName = Prelude.Nothing
    }

-- | Describes the state of association between the IAM role and the DB
-- instance. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
--     can be used to access other AWS services on your behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
--     the DB instance is unable to assume the IAM role in order to access
--     other AWS services on your behalf.
dbInstanceRole_status :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_status = Lens.lens (\DBInstanceRole' {status} -> status) (\s@DBInstanceRole' {} a -> s {status = a} :: DBInstanceRole)

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB instance.
dbInstanceRole_roleArn :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_roleArn = Lens.lens (\DBInstanceRole' {roleArn} -> roleArn) (\s@DBInstanceRole' {} a -> s {roleArn = a} :: DBInstanceRole)

-- | The name of the feature associated with the AWS Identity and Access
-- Management (IAM) role. For the list of supported feature names, see
-- @DBEngineVersion@.
dbInstanceRole_featureName :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_featureName = Lens.lens (\DBInstanceRole' {featureName} -> featureName) (\s@DBInstanceRole' {} a -> s {featureName = a} :: DBInstanceRole)

instance Core.FromXML DBInstanceRole where
  parseXML x =
    DBInstanceRole'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "RoleArn")
      Prelude.<*> (x Core..@? "FeatureName")

instance Prelude.Hashable DBInstanceRole

instance Prelude.NFData DBInstanceRole
