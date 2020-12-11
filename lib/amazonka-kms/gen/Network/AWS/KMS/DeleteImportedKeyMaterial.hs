{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DeleteImportedKeyMaterial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes key material that you previously imported. This operation makes the specified customer master key (CMK) unusable. For more information about importing key material into AWS KMS, see <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html Importing Key Material> in the /AWS Key Management Service Developer Guide/ . You cannot perform this operation on a CMK in a different AWS account.
--
-- When the specified CMK is in the @PendingDeletion@ state, this operation does not change the CMK's state. Otherwise, it changes the CMK's state to @PendingImport@ .
-- After you delete key material, you can use 'ImportKeyMaterial' to reimport the same key material into the CMK.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DeleteImportedKeyMaterial
  ( -- * Creating a request
    DeleteImportedKeyMaterial (..),
    mkDeleteImportedKeyMaterial,

    -- ** Request lenses
    dikmKeyId,

    -- * Destructuring the response
    DeleteImportedKeyMaterialResponse (..),
    mkDeleteImportedKeyMaterialResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteImportedKeyMaterial' smart constructor.
newtype DeleteImportedKeyMaterial = DeleteImportedKeyMaterial'
  { keyId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImportedKeyMaterial' with the minimum fields required to make a request.
--
-- * 'keyId' - Identifies the CMK from which you are deleting imported key material. The @Origin@ of the CMK must be @EXTERNAL@ .
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
mkDeleteImportedKeyMaterial ::
  -- | 'keyId'
  Lude.Text ->
  DeleteImportedKeyMaterial
mkDeleteImportedKeyMaterial pKeyId_ =
  DeleteImportedKeyMaterial' {keyId = pKeyId_}

-- | Identifies the CMK from which you are deleting imported key material. The @Origin@ of the CMK must be @EXTERNAL@ .
--
-- Specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- For example:
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dikmKeyId :: Lens.Lens' DeleteImportedKeyMaterial Lude.Text
dikmKeyId = Lens.lens (keyId :: DeleteImportedKeyMaterial -> Lude.Text) (\s a -> s {keyId = a} :: DeleteImportedKeyMaterial)
{-# DEPRECATED dikmKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.AWSRequest DeleteImportedKeyMaterial where
  type
    Rs DeleteImportedKeyMaterial =
      DeleteImportedKeyMaterialResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull DeleteImportedKeyMaterialResponse'

instance Lude.ToHeaders DeleteImportedKeyMaterial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DeleteImportedKeyMaterial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImportedKeyMaterial where
  toJSON DeleteImportedKeyMaterial' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("KeyId" Lude..= keyId)])

instance Lude.ToPath DeleteImportedKeyMaterial where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImportedKeyMaterial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImportedKeyMaterialResponse' smart constructor.
data DeleteImportedKeyMaterialResponse = DeleteImportedKeyMaterialResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImportedKeyMaterialResponse' with the minimum fields required to make a request.
mkDeleteImportedKeyMaterialResponse ::
  DeleteImportedKeyMaterialResponse
mkDeleteImportedKeyMaterialResponse =
  DeleteImportedKeyMaterialResponse'
