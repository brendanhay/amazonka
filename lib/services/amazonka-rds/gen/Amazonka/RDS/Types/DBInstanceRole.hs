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
-- Module      : Amazonka.RDS.Types.DBInstanceRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBInstanceRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Web Services Identity and Access Management (IAM)
-- role that is associated with a DB instance.
--
-- /See:/ 'newDBInstanceRole' smart constructor.
data DBInstanceRole = DBInstanceRole'
  { -- | The name of the feature associated with the Amazon Web Services Identity
    -- and Access Management (IAM) role. For information about supported
    -- feature names, see @DBEngineVersion@.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that is associated with
    -- the DB instance.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the state of association between the IAM role and the DB
    -- instance. The Status property returns one of the following values:
    --
    -- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
    --     can be used to access other Amazon Web Services services on your
    --     behalf.
    --
    -- -   @PENDING@ - the IAM role ARN is being associated with the DB
    --     instance.
    --
    -- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
    --     the DB instance is unable to assume the IAM role in order to access
    --     other Amazon Web Services services on your behalf.
    status :: Prelude.Maybe Prelude.Text
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
-- 'featureName', 'dbInstanceRole_featureName' - The name of the feature associated with the Amazon Web Services Identity
-- and Access Management (IAM) role. For information about supported
-- feature names, see @DBEngineVersion@.
--
-- 'roleArn', 'dbInstanceRole_roleArn' - The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB instance.
--
-- 'status', 'dbInstanceRole_status' - Describes the state of association between the IAM role and the DB
-- instance. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
--     can be used to access other Amazon Web Services services on your
--     behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
--     the DB instance is unable to assume the IAM role in order to access
--     other Amazon Web Services services on your behalf.
newDBInstanceRole ::
  DBInstanceRole
newDBInstanceRole =
  DBInstanceRole'
    { featureName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the feature associated with the Amazon Web Services Identity
-- and Access Management (IAM) role. For information about supported
-- feature names, see @DBEngineVersion@.
dbInstanceRole_featureName :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_featureName = Lens.lens (\DBInstanceRole' {featureName} -> featureName) (\s@DBInstanceRole' {} a -> s {featureName = a} :: DBInstanceRole)

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with
-- the DB instance.
dbInstanceRole_roleArn :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_roleArn = Lens.lens (\DBInstanceRole' {roleArn} -> roleArn) (\s@DBInstanceRole' {} a -> s {roleArn = a} :: DBInstanceRole)

-- | Describes the state of association between the IAM role and the DB
-- instance. The Status property returns one of the following values:
--
-- -   @ACTIVE@ - the IAM role ARN is associated with the DB instance and
--     can be used to access other Amazon Web Services services on your
--     behalf.
--
-- -   @PENDING@ - the IAM role ARN is being associated with the DB
--     instance.
--
-- -   @INVALID@ - the IAM role ARN is associated with the DB instance, but
--     the DB instance is unable to assume the IAM role in order to access
--     other Amazon Web Services services on your behalf.
dbInstanceRole_status :: Lens.Lens' DBInstanceRole (Prelude.Maybe Prelude.Text)
dbInstanceRole_status = Lens.lens (\DBInstanceRole' {status} -> status) (\s@DBInstanceRole' {} a -> s {status = a} :: DBInstanceRole)

instance Data.FromXML DBInstanceRole where
  parseXML x =
    DBInstanceRole'
      Prelude.<$> (x Data..@? "FeatureName")
      Prelude.<*> (x Data..@? "RoleArn")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable DBInstanceRole where
  hashWithSalt _salt DBInstanceRole' {..} =
    _salt
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DBInstanceRole where
  rnf DBInstanceRole' {..} =
    Prelude.rnf featureName `Prelude.seq`
      Prelude.rnf roleArn `Prelude.seq`
        Prelude.rnf status
