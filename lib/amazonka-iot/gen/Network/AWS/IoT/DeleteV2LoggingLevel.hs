{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteV2LoggingLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logging level.
module Network.AWS.IoT.DeleteV2LoggingLevel
    (
    -- * Creating a request
      DeleteV2LoggingLevel (..)
    , mkDeleteV2LoggingLevel
    -- ** Request lenses
    , dvllTargetType
    , dvllTargetName

    -- * Destructuring the response
    , DeleteV2LoggingLevelResponse (..)
    , mkDeleteV2LoggingLevelResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteV2LoggingLevel' smart constructor.
data DeleteV2LoggingLevel = DeleteV2LoggingLevel'
  { targetType :: Types.LogTargetType
    -- ^ The type of resource for which you are configuring logging. Must be @THING_Group@ .
  , targetName :: Types.LogTargetName
    -- ^ The name of the resource for which you are configuring logging.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteV2LoggingLevel' value with any optional fields omitted.
mkDeleteV2LoggingLevel
    :: Types.LogTargetType -- ^ 'targetType'
    -> Types.LogTargetName -- ^ 'targetName'
    -> DeleteV2LoggingLevel
mkDeleteV2LoggingLevel targetType targetName
  = DeleteV2LoggingLevel'{targetType, targetName}

-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvllTargetType :: Lens.Lens' DeleteV2LoggingLevel Types.LogTargetType
dvllTargetType = Lens.field @"targetType"
{-# INLINEABLE dvllTargetType #-}
{-# DEPRECATED targetType "Use generic-lens or generic-optics with 'targetType' instead"  #-}

-- | The name of the resource for which you are configuring logging.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvllTargetName :: Lens.Lens' DeleteV2LoggingLevel Types.LogTargetName
dvllTargetName = Lens.field @"targetName"
{-# INLINEABLE dvllTargetName #-}
{-# DEPRECATED targetName "Use generic-lens or generic-optics with 'targetName' instead"  #-}

instance Core.ToQuery DeleteV2LoggingLevel where
        toQuery DeleteV2LoggingLevel{..}
          = Core.toQueryPair "targetType" targetType Core.<>
              Core.toQueryPair "targetName" targetName

instance Core.ToHeaders DeleteV2LoggingLevel where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteV2LoggingLevel where
        type Rs DeleteV2LoggingLevel = DeleteV2LoggingLevelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE, Core._rqPath = "/v2LoggingLevel",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteV2LoggingLevelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteV2LoggingLevelResponse' smart constructor.
data DeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteV2LoggingLevelResponse' value with any optional fields omitted.
mkDeleteV2LoggingLevelResponse
    :: DeleteV2LoggingLevelResponse
mkDeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
