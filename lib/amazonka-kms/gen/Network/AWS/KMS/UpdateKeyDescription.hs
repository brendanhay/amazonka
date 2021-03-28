{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.UpdateKeyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of a customer master key (CMK). To see the description of a CMK, use 'DescribeKey' . 
--
-- You cannot perform this operation on a CMK in a different AWS account.
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.UpdateKeyDescription
    (
    -- * Creating a request
      UpdateKeyDescription (..)
    , mkUpdateKeyDescription
    -- ** Request lenses
    , ukdKeyId
    , ukdDescription

    -- * Destructuring the response
    , UpdateKeyDescriptionResponse (..)
    , mkUpdateKeyDescriptionResponse
    ) where

import qualified Network.AWS.KMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateKeyDescription' smart constructor.
data UpdateKeyDescription = UpdateKeyDescription'
  { keyId :: Types.KeyId
    -- ^ A unique identifier for the customer master key (CMK).
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
  , description :: Types.DescriptionType
    -- ^ New description for the CMK.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateKeyDescription' value with any optional fields omitted.
mkUpdateKeyDescription
    :: Types.KeyId -- ^ 'keyId'
    -> Types.DescriptionType -- ^ 'description'
    -> UpdateKeyDescription
mkUpdateKeyDescription keyId description
  = UpdateKeyDescription'{keyId, description}

-- | A unique identifier for the customer master key (CMK).
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
ukdKeyId :: Lens.Lens' UpdateKeyDescription Types.KeyId
ukdKeyId = Lens.field @"keyId"
{-# INLINEABLE ukdKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | New description for the CMK.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ukdDescription :: Lens.Lens' UpdateKeyDescription Types.DescriptionType
ukdDescription = Lens.field @"description"
{-# INLINEABLE ukdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery UpdateKeyDescription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateKeyDescription where
        toHeaders UpdateKeyDescription{..}
          = Core.pure ("X-Amz-Target", "TrentService.UpdateKeyDescription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateKeyDescription where
        toJSON UpdateKeyDescription{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("KeyId" Core..= keyId),
                  Core.Just ("Description" Core..= description)])

instance Core.AWSRequest UpdateKeyDescription where
        type Rs UpdateKeyDescription = UpdateKeyDescriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateKeyDescriptionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateKeyDescriptionResponse' smart constructor.
data UpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateKeyDescriptionResponse' value with any optional fields omitted.
mkUpdateKeyDescriptionResponse
    :: UpdateKeyDescriptionResponse
mkUpdateKeyDescriptionResponse = UpdateKeyDescriptionResponse'
