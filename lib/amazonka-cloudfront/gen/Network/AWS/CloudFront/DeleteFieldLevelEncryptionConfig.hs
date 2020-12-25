{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a field-level encryption configuration.
module Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
  ( -- * Creating a request
    DeleteFieldLevelEncryptionConfig (..),
    mkDeleteFieldLevelEncryptionConfig,

    -- ** Request lenses
    dflecId,
    dflecIfMatch,

    -- * Destructuring the response
    DeleteFieldLevelEncryptionConfigResponse (..),
    mkDeleteFieldLevelEncryptionConfigResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFieldLevelEncryptionConfig' smart constructor.
data DeleteFieldLevelEncryptionConfig = DeleteFieldLevelEncryptionConfig'
  { -- | The ID of the configuration you want to delete from CloudFront.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionConfig' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionConfig ::
  -- | 'id'
  Types.String ->
  DeleteFieldLevelEncryptionConfig
mkDeleteFieldLevelEncryptionConfig id =
  DeleteFieldLevelEncryptionConfig' {id, ifMatch = Core.Nothing}

-- | The ID of the configuration you want to delete from CloudFront.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflecId :: Lens.Lens' DeleteFieldLevelEncryptionConfig Types.String
dflecId = Lens.field @"id"
{-# DEPRECATED dflecId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when retrieving the configuration identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dflecIfMatch :: Lens.Lens' DeleteFieldLevelEncryptionConfig (Core.Maybe Types.String)
dflecIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED dflecIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest DeleteFieldLevelEncryptionConfig where
  type
    Rs DeleteFieldLevelEncryptionConfig =
      DeleteFieldLevelEncryptionConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/field-level-encryption/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteFieldLevelEncryptionConfigResponse'

-- | /See:/ 'mkDeleteFieldLevelEncryptionConfigResponse' smart constructor.
data DeleteFieldLevelEncryptionConfigResponse = DeleteFieldLevelEncryptionConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFieldLevelEncryptionConfigResponse' value with any optional fields omitted.
mkDeleteFieldLevelEncryptionConfigResponse ::
  DeleteFieldLevelEncryptionConfigResponse
mkDeleteFieldLevelEncryptionConfigResponse =
  DeleteFieldLevelEncryptionConfigResponse'
