{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KMS.DeleteImportedKeyMaterial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes key material that you previously imported. This operation makes
-- the specified customer master key (CMK) unusable. For more information
-- about importing key material into AWS KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material>
-- in the /AWS Key Management Service Developer Guide/.
--
-- When the specified CMK is in the @PendingDeletion@ state, this operation
-- does not change the CMK\'s state. Otherwise, it changes the CMK\'s state
-- to @PendingImport@.
--
-- After you delete key material, you can use ImportKeyMaterial to reimport
-- the same key material into the CMK.
--
-- The CMK that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key>
-- in the /AWS Key Management Service Developer Guide/.
--
-- __Cross-account use__: No. You cannot perform this operation on a CMK in
-- a different AWS account.
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
module Network.AWS.KMS.DeleteImportedKeyMaterial
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

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteImportedKeyMaterial' smart constructor.
data DeleteImportedKeyMaterial = DeleteImportedKeyMaterial'
  { -- | Identifies the CMK from which you are deleting imported key material.
    -- The @Origin@ of the CMK must be @EXTERNAL@.
    --
    -- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    --
    -- For example:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteImportedKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'deleteImportedKeyMaterial_keyId' - Identifies the CMK from which you are deleting imported key material.
-- The @Origin@ of the CMK must be @EXTERNAL@.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
newDeleteImportedKeyMaterial ::
  -- | 'keyId'
  Prelude.Text ->
  DeleteImportedKeyMaterial
newDeleteImportedKeyMaterial pKeyId_ =
  DeleteImportedKeyMaterial' {keyId = pKeyId_}

-- | Identifies the CMK from which you are deleting imported key material.
-- The @Origin@ of the CMK must be @EXTERNAL@.
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
--
-- For example:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- To get the key ID and key ARN for a CMK, use ListKeys or DescribeKey.
deleteImportedKeyMaterial_keyId :: Lens.Lens' DeleteImportedKeyMaterial Prelude.Text
deleteImportedKeyMaterial_keyId = Lens.lens (\DeleteImportedKeyMaterial' {keyId} -> keyId) (\s@DeleteImportedKeyMaterial' {} a -> s {keyId = a} :: DeleteImportedKeyMaterial)

instance Prelude.AWSRequest DeleteImportedKeyMaterial where
  type
    Rs DeleteImportedKeyMaterial =
      DeleteImportedKeyMaterialResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteImportedKeyMaterialResponse'

instance Prelude.Hashable DeleteImportedKeyMaterial

instance Prelude.NFData DeleteImportedKeyMaterial

instance Prelude.ToHeaders DeleteImportedKeyMaterial where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "TrentService.DeleteImportedKeyMaterial" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteImportedKeyMaterial where
  toJSON DeleteImportedKeyMaterial' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("KeyId" Prelude..= keyId)]
      )

instance Prelude.ToPath DeleteImportedKeyMaterial where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteImportedKeyMaterial where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImportedKeyMaterialResponse' smart constructor.
data DeleteImportedKeyMaterialResponse = DeleteImportedKeyMaterialResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
