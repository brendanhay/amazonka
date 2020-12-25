{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteFieldLevelEncryptionProfile (..),
    mkDeleteFieldLevelEncryptionProfile,

    -- ** Request lenses
    dflepId,
    dflepIfMatch,

    -- * Destructuring the response
    DeleteFieldLevelEncryptionProfileResponse (..),
    mkDeleteFieldLevelEncryptionProfileResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfile' smart constructor.
data DeleteFieldLevelEncryptionProfile = DeleteFieldLevelEncryptionProfile'
  { -- | Request the ID of the profile you want to delete from CloudFront.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionProfile' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionProfile ::
  -- | 'id'
  Types.String ->
  DeleteFieldLevelEncryptionProfile
mkDeleteFieldLevelEncryptionProfile id =
  DeleteFieldLevelEncryptionProfile' {id, ifMatch = Core.Nothing}

-- | Request the ID of the profile you want to delete from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepId :: Lens.Lens' DeleteFieldLevelEncryptionProfile Types.String
dflepId = Lens.field @"id"
{-# DEPRECATED dflepId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when retrieving the profile to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflepIfMatch :: Lens.Lens' DeleteFieldLevelEncryptionProfile (Core.Maybe Types.String)
dflepIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED dflepIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest DeleteFieldLevelEncryptionProfile where
  type
    Rs DeleteFieldLevelEncryptionProfile =
      DeleteFieldLevelEncryptionProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/field-level-encryption-profile/"
                Core.<> (Core.toText id)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteFieldLevelEncryptionProfileResponse'

-- | /See:/ 'mkDeleteFieldLevelEncryptionProfileResponse' smart constructor.
data DeleteFieldLevelEncryptionProfileResponse = DeleteFieldLevelEncryptionProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionProfileResponse' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionProfileResponse ::
  DeleteFieldLevelEncryptionProfileResponse
mkDeleteFieldLevelEncryptionProfileResponse =
  DeleteFieldLevelEncryptionProfileResponse'
