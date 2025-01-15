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
-- Module      : Amazonka.RDS.Types.MasterUserSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.MasterUserSecret where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the secret managed by RDS in Amazon Web Services Secrets
-- Manager for the master user password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- /See:/ 'newMasterUserSecret' smart constructor.
data MasterUserSecret = MasterUserSecret'
  { -- | The Amazon Web Services KMS key identifier that is used to encrypt the
    -- secret.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the secret.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the secret.
    --
    -- The possible status values include the following:
    --
    -- -   @creating@ - The secret is being created.
    --
    -- -   @active@ - The secret is available for normal use and rotation.
    --
    -- -   @rotating@ - The secret is being rotated.
    --
    -- -   @impaired@ - The secret can be used to access database credentials,
    --     but it can\'t be rotated. A secret might have this status if, for
    --     example, permissions are changed so that RDS can no longer access
    --     either the secret or the KMS key for the secret.
    --
    --     When a secret has this status, you can correct the condition that
    --     caused the status. Alternatively, modify the DB instance to turn off
    --     automatic management of database credentials, and then modify the DB
    --     instance again to turn on automatic management of database
    --     credentials.
    secretStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MasterUserSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'masterUserSecret_kmsKeyId' - The Amazon Web Services KMS key identifier that is used to encrypt the
-- secret.
--
-- 'secretArn', 'masterUserSecret_secretArn' - The Amazon Resource Name (ARN) of the secret.
--
-- 'secretStatus', 'masterUserSecret_secretStatus' - The status of the secret.
--
-- The possible status values include the following:
--
-- -   @creating@ - The secret is being created.
--
-- -   @active@ - The secret is available for normal use and rotation.
--
-- -   @rotating@ - The secret is being rotated.
--
-- -   @impaired@ - The secret can be used to access database credentials,
--     but it can\'t be rotated. A secret might have this status if, for
--     example, permissions are changed so that RDS can no longer access
--     either the secret or the KMS key for the secret.
--
--     When a secret has this status, you can correct the condition that
--     caused the status. Alternatively, modify the DB instance to turn off
--     automatic management of database credentials, and then modify the DB
--     instance again to turn on automatic management of database
--     credentials.
newMasterUserSecret ::
  MasterUserSecret
newMasterUserSecret =
  MasterUserSecret'
    { kmsKeyId = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      secretStatus = Prelude.Nothing
    }

-- | The Amazon Web Services KMS key identifier that is used to encrypt the
-- secret.
masterUserSecret_kmsKeyId :: Lens.Lens' MasterUserSecret (Prelude.Maybe Prelude.Text)
masterUserSecret_kmsKeyId = Lens.lens (\MasterUserSecret' {kmsKeyId} -> kmsKeyId) (\s@MasterUserSecret' {} a -> s {kmsKeyId = a} :: MasterUserSecret)

-- | The Amazon Resource Name (ARN) of the secret.
masterUserSecret_secretArn :: Lens.Lens' MasterUserSecret (Prelude.Maybe Prelude.Text)
masterUserSecret_secretArn = Lens.lens (\MasterUserSecret' {secretArn} -> secretArn) (\s@MasterUserSecret' {} a -> s {secretArn = a} :: MasterUserSecret)

-- | The status of the secret.
--
-- The possible status values include the following:
--
-- -   @creating@ - The secret is being created.
--
-- -   @active@ - The secret is available for normal use and rotation.
--
-- -   @rotating@ - The secret is being rotated.
--
-- -   @impaired@ - The secret can be used to access database credentials,
--     but it can\'t be rotated. A secret might have this status if, for
--     example, permissions are changed so that RDS can no longer access
--     either the secret or the KMS key for the secret.
--
--     When a secret has this status, you can correct the condition that
--     caused the status. Alternatively, modify the DB instance to turn off
--     automatic management of database credentials, and then modify the DB
--     instance again to turn on automatic management of database
--     credentials.
masterUserSecret_secretStatus :: Lens.Lens' MasterUserSecret (Prelude.Maybe Prelude.Text)
masterUserSecret_secretStatus = Lens.lens (\MasterUserSecret' {secretStatus} -> secretStatus) (\s@MasterUserSecret' {} a -> s {secretStatus = a} :: MasterUserSecret)

instance Data.FromXML MasterUserSecret where
  parseXML x =
    MasterUserSecret'
      Prelude.<$> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "SecretArn")
      Prelude.<*> (x Data..@? "SecretStatus")

instance Prelude.Hashable MasterUserSecret where
  hashWithSalt _salt MasterUserSecret' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` secretStatus

instance Prelude.NFData MasterUserSecret where
  rnf MasterUserSecret' {..} =
    Prelude.rnf kmsKeyId `Prelude.seq`
      Prelude.rnf secretArn `Prelude.seq`
        Prelude.rnf secretStatus
