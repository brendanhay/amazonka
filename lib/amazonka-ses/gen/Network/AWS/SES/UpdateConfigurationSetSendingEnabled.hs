{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetSendingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending for messages sent using a specific configuration set in a given AWS Region. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) exceed certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateConfigurationSetSendingEnabled
    (
    -- * Creating a request
      UpdateConfigurationSetSendingEnabled (..)
    , mkUpdateConfigurationSetSendingEnabled
    -- ** Request lenses
    , ucsseConfigurationSetName
    , ucsseEnabled

    -- * Destructuring the response
    , UpdateConfigurationSetSendingEnabledResponse (..)
    , mkUpdateConfigurationSetSendingEnabledResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to enable or disable the email sending capabilities for a specific configuration set.
--
-- /See:/ 'mkUpdateConfigurationSetSendingEnabled' smart constructor.
data UpdateConfigurationSetSendingEnabled = UpdateConfigurationSetSendingEnabled'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set that you want to update.
  , enabled :: Core.Bool
    -- ^ Describes whether email sending is enabled or disabled for the configuration set. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetSendingEnabled' value with any optional fields omitted.
mkUpdateConfigurationSetSendingEnabled
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> Core.Bool -- ^ 'enabled'
    -> UpdateConfigurationSetSendingEnabled
mkUpdateConfigurationSetSendingEnabled configurationSetName enabled
  = UpdateConfigurationSetSendingEnabled'{configurationSetName,
                                          enabled}

-- | The name of the configuration set that you want to update.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsseConfigurationSetName :: Lens.Lens' UpdateConfigurationSetSendingEnabled Types.ConfigurationSetName
ucsseConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE ucsseConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | Describes whether email sending is enabled or disabled for the configuration set. 
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsseEnabled :: Lens.Lens' UpdateConfigurationSetSendingEnabled Core.Bool
ucsseEnabled = Lens.field @"enabled"
{-# INLINEABLE ucsseEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery UpdateConfigurationSetSendingEnabled where
        toQuery UpdateConfigurationSetSendingEnabled{..}
          = Core.toQueryPair "Action"
              ("UpdateConfigurationSetSendingEnabled" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<> Core.toQueryPair "Enabled" enabled

instance Core.ToHeaders UpdateConfigurationSetSendingEnabled where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UpdateConfigurationSetSendingEnabled where
        type Rs UpdateConfigurationSetSendingEnabled =
             UpdateConfigurationSetSendingEnabledResponse
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
          = Response.receiveNull
              UpdateConfigurationSetSendingEnabledResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateConfigurationSetSendingEnabledResponse' smart constructor.
data UpdateConfigurationSetSendingEnabledResponse = UpdateConfigurationSetSendingEnabledResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetSendingEnabledResponse' value with any optional fields omitted.
mkUpdateConfigurationSetSendingEnabledResponse
    :: UpdateConfigurationSetSendingEnabledResponse
mkUpdateConfigurationSetSendingEnabledResponse
  = UpdateConfigurationSetSendingEnabledResponse'
