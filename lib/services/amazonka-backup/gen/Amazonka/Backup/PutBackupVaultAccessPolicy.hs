{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.PutBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a resource-based policy that is used to manage access permissions
-- on the target backup vault. Requires a backup vault name and an access
-- policy document in JSON format.
module Amazonka.Backup.PutBackupVaultAccessPolicy
  ( -- * Creating a Request
    PutBackupVaultAccessPolicy (..),
    newPutBackupVaultAccessPolicy,

    -- * Request Lenses
    putBackupVaultAccessPolicy_policy,
    putBackupVaultAccessPolicy_backupVaultName,

    -- * Destructuring the Response
    PutBackupVaultAccessPolicyResponse (..),
    newPutBackupVaultAccessPolicyResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBackupVaultAccessPolicy' smart constructor.
data PutBackupVaultAccessPolicy = PutBackupVaultAccessPolicy'
  { -- | The backup vault access policy document in JSON format.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putBackupVaultAccessPolicy_policy' - The backup vault access policy document in JSON format.
--
-- 'backupVaultName', 'putBackupVaultAccessPolicy_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newPutBackupVaultAccessPolicy ::
  -- | 'backupVaultName'
  Prelude.Text ->
  PutBackupVaultAccessPolicy
newPutBackupVaultAccessPolicy pBackupVaultName_ =
  PutBackupVaultAccessPolicy'
    { policy =
        Prelude.Nothing,
      backupVaultName = pBackupVaultName_
    }

-- | The backup vault access policy document in JSON format.
putBackupVaultAccessPolicy_policy :: Lens.Lens' PutBackupVaultAccessPolicy (Prelude.Maybe Prelude.Text)
putBackupVaultAccessPolicy_policy = Lens.lens (\PutBackupVaultAccessPolicy' {policy} -> policy) (\s@PutBackupVaultAccessPolicy' {} a -> s {policy = a} :: PutBackupVaultAccessPolicy)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
putBackupVaultAccessPolicy_backupVaultName :: Lens.Lens' PutBackupVaultAccessPolicy Prelude.Text
putBackupVaultAccessPolicy_backupVaultName = Lens.lens (\PutBackupVaultAccessPolicy' {backupVaultName} -> backupVaultName) (\s@PutBackupVaultAccessPolicy' {} a -> s {backupVaultName = a} :: PutBackupVaultAccessPolicy)

instance Core.AWSRequest PutBackupVaultAccessPolicy where
  type
    AWSResponse PutBackupVaultAccessPolicy =
      PutBackupVaultAccessPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutBackupVaultAccessPolicyResponse'

instance Prelude.Hashable PutBackupVaultAccessPolicy where
  hashWithSalt _salt PutBackupVaultAccessPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` backupVaultName

instance Prelude.NFData PutBackupVaultAccessPolicy where
  rnf PutBackupVaultAccessPolicy' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf backupVaultName

instance Data.ToHeaders PutBackupVaultAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutBackupVaultAccessPolicy where
  toJSON PutBackupVaultAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Policy" Data..=) Prelude.<$> policy]
      )

instance Data.ToPath PutBackupVaultAccessPolicy where
  toPath PutBackupVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/access-policy"
      ]

instance Data.ToQuery PutBackupVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBackupVaultAccessPolicyResponse' smart constructor.
data PutBackupVaultAccessPolicyResponse = PutBackupVaultAccessPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBackupVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBackupVaultAccessPolicyResponse ::
  PutBackupVaultAccessPolicyResponse
newPutBackupVaultAccessPolicyResponse =
  PutBackupVaultAccessPolicyResponse'

instance
  Prelude.NFData
    PutBackupVaultAccessPolicyResponse
  where
  rnf _ = ()
