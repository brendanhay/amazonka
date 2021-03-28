{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption profile.
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
    (
    -- * Creating a request
      DeleteFieldLevelEncryptionProfile (..)
    , mkDeleteFieldLevelEncryptionProfile
    -- ** Request lenses
    , dflepId
    , dflepIfMatch

    -- * Destructuring the response
    , DeleteFieldLevelEncryptionProfileResponse (..)
    , mkDeleteFieldLevelEncryptionProfileResponse
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfile' smart constructor.
data DeleteFieldLevelEncryptionProfile = DeleteFieldLevelEncryptionProfile'
  { id :: Core.Text
    -- ^ Request the ID of the profile you want to delete from CloudFront.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionProfile' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionProfile
    :: Core.Text -- ^ 'id'
    -> DeleteFieldLevelEncryptionProfile
mkDeleteFieldLevelEncryptionProfile id
  = DeleteFieldLevelEncryptionProfile'{id, ifMatch = Core.Nothing}

-- | Request the ID of the profile you want to delete from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepId :: Lens.Lens' DeleteFieldLevelEncryptionProfile Core.Text
dflepId = Lens.field @"id"
{-# INLINEABLE dflepId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepIfMatch :: Lens.Lens' DeleteFieldLevelEncryptionProfile (Core.Maybe Core.Text)
dflepIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE dflepIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery DeleteFieldLevelEncryptionProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFieldLevelEncryptionProfile where
        toHeaders DeleteFieldLevelEncryptionProfile{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest DeleteFieldLevelEncryptionProfile where
        type Rs DeleteFieldLevelEncryptionProfile =
             DeleteFieldLevelEncryptionProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2020-05-31/field-level-encryption-profile/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteFieldLevelEncryptionProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfileResponse' smart constructor.
data DeleteFieldLevelEncryptionProfileResponse = DeleteFieldLevelEncryptionProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionProfileResponse
    :: DeleteFieldLevelEncryptionProfileResponse
mkDeleteFieldLevelEncryptionProfileResponse
  = DeleteFieldLevelEncryptionProfileResponse'
