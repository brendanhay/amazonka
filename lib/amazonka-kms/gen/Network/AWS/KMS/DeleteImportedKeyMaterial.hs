{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteImportedKeyMaterial (..)
    , mkDeleteImportedKeyMaterial
    -- ** Request lenses
    , dikmKeyId

    -- * Destructuring the response
    , DeleteImportedKeyMaterialResponse (..)
    , mkDeleteImportedKeyMaterialResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteImportedKeyMaterial' smart constructor.
newtype DeleteImportedKeyMaterial = DeleteImportedKeyMaterial'
  { keyId :: Types.KeyIdType
    -- ^ Identifies the CMK from which you are deleting imported key material. The @Origin@ of the CMK must be @EXTERNAL@ .
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImportedKeyMaterial' value with any optional fields omitted.
mkDeleteImportedKeyMaterial
    :: Types.KeyIdType -- ^ 'keyId'
    -> DeleteImportedKeyMaterial
mkDeleteImportedKeyMaterial keyId
  = DeleteImportedKeyMaterial'{keyId}

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
dikmKeyId :: Lens.Lens' DeleteImportedKeyMaterial Types.KeyIdType
dikmKeyId = Lens.field @"keyId"
{-# INLINEABLE dikmKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToQuery DeleteImportedKeyMaterial where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteImportedKeyMaterial where
        toHeaders DeleteImportedKeyMaterial{..}
          = Core.pure
              ("X-Amz-Target", "TrentService.DeleteImportedKeyMaterial")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteImportedKeyMaterial where
        toJSON DeleteImportedKeyMaterial{..}
          = Core.object (Core.catMaybes [Core.Just ("KeyId" Core..= keyId)])

instance Core.AWSRequest DeleteImportedKeyMaterial where
        type Rs DeleteImportedKeyMaterial =
             DeleteImportedKeyMaterialResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteImportedKeyMaterialResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteImportedKeyMaterialResponse' smart constructor.
data DeleteImportedKeyMaterialResponse = DeleteImportedKeyMaterialResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImportedKeyMaterialResponse' value with any optional fields omitted.
mkDeleteImportedKeyMaterialResponse
    :: DeleteImportedKeyMaterialResponse
mkDeleteImportedKeyMaterialResponse
  = DeleteImportedKeyMaterialResponse'
