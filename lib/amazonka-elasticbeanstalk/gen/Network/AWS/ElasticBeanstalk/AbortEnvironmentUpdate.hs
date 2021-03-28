{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application version deployment.
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
    (
    -- * Creating a request
      AbortEnvironmentUpdate (..)
    , mkAbortEnvironmentUpdate
    -- ** Request lenses
    , aeuEnvironmentId
    , aeuEnvironmentName

    -- * Destructuring the response
    , AbortEnvironmentUpdateResponse (..)
    , mkAbortEnvironmentUpdateResponse
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkAbortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { environmentId :: Core.Maybe Types.EnvironmentId
    -- ^ This specifies the ID of the environment with the in-progress update that you want to cancel.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ This specifies the name of the environment with the in-progress update that you want to cancel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortEnvironmentUpdate' value with any optional fields omitted.
mkAbortEnvironmentUpdate
    :: AbortEnvironmentUpdate
mkAbortEnvironmentUpdate
  = AbortEnvironmentUpdate'{environmentId = Core.Nothing,
                            environmentName = Core.Nothing}

-- | This specifies the ID of the environment with the in-progress update that you want to cancel.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuEnvironmentId :: Lens.Lens' AbortEnvironmentUpdate (Core.Maybe Types.EnvironmentId)
aeuEnvironmentId = Lens.field @"environmentId"
{-# INLINEABLE aeuEnvironmentId #-}
{-# DEPRECATED environmentId "Use generic-lens or generic-optics with 'environmentId' instead"  #-}

-- | This specifies the name of the environment with the in-progress update that you want to cancel.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeuEnvironmentName :: Lens.Lens' AbortEnvironmentUpdate (Core.Maybe Types.EnvironmentName)
aeuEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE aeuEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

instance Core.ToQuery AbortEnvironmentUpdate where
        toQuery AbortEnvironmentUpdate{..}
          = Core.toQueryPair "Action" ("AbortEnvironmentUpdate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentId")
                environmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnvironmentName")
                environmentName

instance Core.ToHeaders AbortEnvironmentUpdate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AbortEnvironmentUpdate where
        type Rs AbortEnvironmentUpdate = AbortEnvironmentUpdateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AbortEnvironmentUpdateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAbortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortEnvironmentUpdateResponse' value with any optional fields omitted.
mkAbortEnvironmentUpdateResponse
    :: AbortEnvironmentUpdateResponse
mkAbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
