{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Swaps the CNAMEs of two environments.
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
  ( -- * Creating a request
    SwapEnvironmentCNAMEs (..),
    mkSwapEnvironmentCNAMEs,

    -- ** Request lenses
    secnameDestinationEnvironmentId,
    secnameDestinationEnvironmentName,
    secnameSourceEnvironmentId,
    secnameSourceEnvironmentName,

    -- * Destructuring the response
    SwapEnvironmentCNAMEsResponse (..),
    mkSwapEnvironmentCNAMEsResponse,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Swaps the CNAMEs of two environments.
--
-- /See:/ 'mkSwapEnvironmentCNAMEs' smart constructor.
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'
  { -- | The ID of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
    destinationEnvironmentId :: Core.Maybe Types.DestinationEnvironmentId,
    -- | The name of the destination environment.
    --
    -- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
    destinationEnvironmentName :: Core.Maybe Types.DestinationEnvironmentName,
    -- | The ID of the source environment.
    --
    -- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
    sourceEnvironmentId :: Core.Maybe Types.SourceEnvironmentId,
    -- | The name of the source environment.
    --
    -- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
    sourceEnvironmentName :: Core.Maybe Types.SourceEnvironmentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SwapEnvironmentCNAMEs' value with any optional fields omitted.
mkSwapEnvironmentCNAMEs ::
  SwapEnvironmentCNAMEs
mkSwapEnvironmentCNAMEs =
  SwapEnvironmentCNAMEs'
    { destinationEnvironmentId = Core.Nothing,
      destinationEnvironmentName = Core.Nothing,
      sourceEnvironmentId = Core.Nothing,
      sourceEnvironmentName = Core.Nothing
    }

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
--
-- /Note:/ Consider using 'destinationEnvironmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameDestinationEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Types.DestinationEnvironmentId)
secnameDestinationEnvironmentId = Lens.field @"destinationEnvironmentId"
{-# DEPRECATED secnameDestinationEnvironmentId "Use generic-lens or generic-optics with 'destinationEnvironmentId' instead." #-}

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
--
-- /Note:/ Consider using 'destinationEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameDestinationEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Types.DestinationEnvironmentName)
secnameDestinationEnvironmentName = Lens.field @"destinationEnvironmentName"
{-# DEPRECATED secnameDestinationEnvironmentName "Use generic-lens or generic-optics with 'destinationEnvironmentName' instead." #-}

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
--
-- /Note:/ Consider using 'sourceEnvironmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameSourceEnvironmentId :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Types.SourceEnvironmentId)
secnameSourceEnvironmentId = Lens.field @"sourceEnvironmentId"
{-# DEPRECATED secnameSourceEnvironmentId "Use generic-lens or generic-optics with 'sourceEnvironmentId' instead." #-}

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
--
-- /Note:/ Consider using 'sourceEnvironmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
secnameSourceEnvironmentName :: Lens.Lens' SwapEnvironmentCNAMEs (Core.Maybe Types.SourceEnvironmentName)
secnameSourceEnvironmentName = Lens.field @"sourceEnvironmentName"
{-# DEPRECATED secnameSourceEnvironmentName "Use generic-lens or generic-optics with 'sourceEnvironmentName' instead." #-}

instance Core.AWSRequest SwapEnvironmentCNAMEs where
  type Rs SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SwapEnvironmentCNAMEs")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> ( Core.toQueryValue "DestinationEnvironmentId"
                            Core.<$> destinationEnvironmentId
                        )
                Core.<> ( Core.toQueryValue "DestinationEnvironmentName"
                            Core.<$> destinationEnvironmentName
                        )
                Core.<> ( Core.toQueryValue "SourceEnvironmentId"
                            Core.<$> sourceEnvironmentId
                        )
                Core.<> ( Core.toQueryValue "SourceEnvironmentName"
                            Core.<$> sourceEnvironmentName
                        )
            )
      }
  response = Response.receiveNull SwapEnvironmentCNAMEsResponse'

-- | /See:/ 'mkSwapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SwapEnvironmentCNAMEsResponse' value with any optional fields omitted.
mkSwapEnvironmentCNAMEsResponse ::
  SwapEnvironmentCNAMEsResponse
mkSwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
