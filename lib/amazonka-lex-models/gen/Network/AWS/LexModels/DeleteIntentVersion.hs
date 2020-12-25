{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteIntentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of an intent. To delete all versions of a intent, use the 'DeleteIntent' operation.
--
-- This operation requires permissions for the @lex:DeleteIntentVersion@ action.
module Network.AWS.LexModels.DeleteIntentVersion
  ( -- * Creating a request
    DeleteIntentVersion (..),
    mkDeleteIntentVersion,

    -- ** Request lenses
    divName,
    divVersion,

    -- * Destructuring the response
    DeleteIntentVersionResponse (..),
    mkDeleteIntentVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { -- | The name of the intent.
    name :: Types.IntentName,
    -- | The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
    version :: Types.NumericalVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntentVersion' value with any optional fields omitted.
mkDeleteIntentVersion ::
  -- | 'name'
  Types.IntentName ->
  -- | 'version'
  Types.NumericalVersion ->
  DeleteIntentVersion
mkDeleteIntentVersion name version =
  DeleteIntentVersion' {name, version}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divName :: Lens.Lens' DeleteIntentVersion Types.IntentName
divName = Lens.field @"name"
{-# DEPRECATED divName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divVersion :: Lens.Lens' DeleteIntentVersion Types.NumericalVersion
divVersion = Lens.field @"version"
{-# DEPRECATED divVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.AWSRequest DeleteIntentVersion where
  type Rs DeleteIntentVersion = DeleteIntentVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/intents/" Core.<> (Core.toText name) Core.<> ("/versions/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteIntentVersionResponse'

-- | /See:/ 'mkDeleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse = DeleteIntentVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIntentVersionResponse' value with any optional fields omitted.
mkDeleteIntentVersionResponse ::
  DeleteIntentVersionResponse
mkDeleteIntentVersionResponse = DeleteIntentVersionResponse'
