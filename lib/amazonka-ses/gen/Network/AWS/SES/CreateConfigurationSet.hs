{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set.
--
-- Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateConfigurationSet
    (
    -- * Creating a request
      CreateConfigurationSet (..)
    , mkCreateConfigurationSet
    -- ** Request lenses
    , ccsConfigurationSet

    -- * Destructuring the response
    , CreateConfigurationSetResponse (..)
    , mkCreateConfigurationSetResponse
    -- ** Response lenses
    , ccsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to create a configuration set. Configuration sets enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateConfigurationSet' smart constructor.
newtype CreateConfigurationSet = CreateConfigurationSet'
  { configurationSet :: Types.ConfigurationSet
    -- ^ A data structure that contains the name of the configuration set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSet' value with any optional fields omitted.
mkCreateConfigurationSet
    :: Types.ConfigurationSet -- ^ 'configurationSet'
    -> CreateConfigurationSet
mkCreateConfigurationSet configurationSet
  = CreateConfigurationSet'{configurationSet}

-- | A data structure that contains the name of the configuration set.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsConfigurationSet :: Lens.Lens' CreateConfigurationSet Types.ConfigurationSet
ccsConfigurationSet = Lens.field @"configurationSet"
{-# INLINEABLE ccsConfigurationSet #-}
{-# DEPRECATED configurationSet "Use generic-lens or generic-optics with 'configurationSet' instead"  #-}

instance Core.ToQuery CreateConfigurationSet where
        toQuery CreateConfigurationSet{..}
          = Core.toQueryPair "Action" ("CreateConfigurationSet" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ConfigurationSet" configurationSet

instance Core.ToHeaders CreateConfigurationSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateConfigurationSet where
        type Rs CreateConfigurationSet = CreateConfigurationSetResponse
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
          = Response.receiveXMLWrapper "CreateConfigurationSetResult"
              (\ s h x ->
                 CreateConfigurationSetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetResponse' smart constructor.
newtype CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateConfigurationSetResponse' value with any optional fields omitted.
mkCreateConfigurationSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateConfigurationSetResponse
mkCreateConfigurationSetResponse responseStatus
  = CreateConfigurationSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrrsResponseStatus :: Lens.Lens' CreateConfigurationSetResponse Core.Int
ccsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
