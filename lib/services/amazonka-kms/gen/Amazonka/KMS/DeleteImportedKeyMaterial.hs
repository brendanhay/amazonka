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
-- Module      : Amazonka.KMS.DeleteImportedKeyMaterial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes key material that was previously imported. This operation makes
-- the specified KMS key temporarily unusable. To restore the usability of
-- the KMS key, reimport the same key material. For more information about
-- importing key material into KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /Key Management Service Developer Guide/.
--
-- When the specified KMS key is in the @PendingDeletion@ state, this
-- operation does not change the KMS key\'s state. Otherwise, it changes
-- the KMS key\'s state to @PendingImport@.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a KMS
-- key in a different Amazon Web Services account.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:DeleteImportedKeyMaterial>
-- (key policy)
--
-- __Related operations:__
--
-- -   GetParametersForImport
--
-- -   ImportKeyMaterial
module Amazonka.KMS.DeleteImportedKeyMaterial
  ( -- * Creating a Request
    DeleteImportedKeyMaterial (..),
    newDeleteImportedKeyMaterial,

    -- * Request Lenses
    deleteImportedKeyMaterial_keyId,

    -- * Destructuring the Response
    DeleteImportedKeyMaterialResponse (..),
    newDeleteImportedKeyMaterialResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImportedKeyMaterial' smart constructor.
data DeleteImportedKeyMaterial = DeleteImportedKeyMaterial'
  { -- | Identifies the KMS key from which you are deleting imported key
    -- material. The @Origin@ of the KMS key must be @EXTERNAL@.
    --
    -- Specify the key ID or key ARN of the KMS key.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a KMS key, use ListKeys or
    -- DescribeKey.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImportedKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'deleteImportedKeyMaterial_keyId' - Identifies the KMS key from which you are deleting imported key
-- material. The @Origin@ of the KMS key must be @EXTERNAL@.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
newDeleteImportedKeyMaterial ::
  -- | 'keyId'
  Prelude.Text ->
  DeleteImportedKeyMaterial
newDeleteImportedKeyMaterial pKeyId_ =
  DeleteImportedKeyMaterial' {keyId = pKeyId_}

-- | Identifies the KMS key from which you are deleting imported key
-- material. The @Origin@ of the KMS key must be @EXTERNAL@.
--
-- Specify the key ID or key ARN of the KMS key.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a KMS key, use ListKeys or
-- DescribeKey.
deleteImportedKeyMaterial_keyId :: Lens.Lens' DeleteImportedKeyMaterial Prelude.Text
deleteImportedKeyMaterial_keyId = Lens.lens (\DeleteImportedKeyMaterial' {keyId} -> keyId) (\s@DeleteImportedKeyMaterial' {} a -> s {keyId = a} :: DeleteImportedKeyMaterial)

instance Core.AWSRequest DeleteImportedKeyMaterial where
  type
    AWSResponse DeleteImportedKeyMaterial =
      DeleteImportedKeyMaterialResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteImportedKeyMaterialResponse'

instance Prelude.Hashable DeleteImportedKeyMaterial where
  hashWithSalt _salt DeleteImportedKeyMaterial' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData DeleteImportedKeyMaterial where
  rnf DeleteImportedKeyMaterial' {..} =
    Prelude.rnf keyId

instance Data.ToHeaders DeleteImportedKeyMaterial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TrentService.DeleteImportedKeyMaterial" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImportedKeyMaterial where
  toJSON DeleteImportedKeyMaterial' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Data..= keyId)]
      )

instance Data.ToPath DeleteImportedKeyMaterial where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImportedKeyMaterial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImportedKeyMaterialResponse' smart constructor.
data DeleteImportedKeyMaterialResponse = DeleteImportedKeyMaterialResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImportedKeyMaterialResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteImportedKeyMaterialResponse ::
  DeleteImportedKeyMaterialResponse
newDeleteImportedKeyMaterialResponse =
  DeleteImportedKeyMaterialResponse'

instance
  Prelude.NFData
    DeleteImportedKeyMaterialResponse
  where
  rnf _ = ()
