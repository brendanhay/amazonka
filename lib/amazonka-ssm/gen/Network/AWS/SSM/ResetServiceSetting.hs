{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ResetServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'GetServiceSetting' API action to view the current value. Use the 'UpdateServiceSetting' API action to change the default setting. 
-- Reset the service setting for the account to the default value as provisioned by the AWS service team. 
module Network.AWS.SSM.ResetServiceSetting
    (
    -- * Creating a request
      ResetServiceSetting (..)
    , mkResetServiceSetting
    -- ** Request lenses
    , rssSettingId

    -- * Destructuring the response
    , ResetServiceSettingResponse (..)
    , mkResetServiceSettingResponse
    -- ** Response lenses
    , rssrrsServiceSetting
    , rssrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | The request body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSetting' smart constructor.
newtype ResetServiceSetting = ResetServiceSetting'
  { settingId :: Types.SettingId
    -- ^ The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetServiceSetting' value with any optional fields omitted.
mkResetServiceSetting
    :: Types.SettingId -- ^ 'settingId'
    -> ResetServiceSetting
mkResetServiceSetting settingId = ResetServiceSetting'{settingId}

-- | The Amazon Resource Name (ARN) of the service setting to reset. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ . For example, @arn:aws:ssm:us-east-1:111122223333:servicesetting/ssm/parameter-store/high-throughput-enabled@ .
--
-- /Note:/ Consider using 'settingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssSettingId :: Lens.Lens' ResetServiceSetting Types.SettingId
rssSettingId = Lens.field @"settingId"
{-# INLINEABLE rssSettingId #-}
{-# DEPRECATED settingId "Use generic-lens or generic-optics with 'settingId' instead"  #-}

instance Core.ToQuery ResetServiceSetting where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetServiceSetting where
        toHeaders ResetServiceSetting{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ResetServiceSetting")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetServiceSetting where
        toJSON ResetServiceSetting{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SettingId" Core..= settingId)])

instance Core.AWSRequest ResetServiceSetting where
        type Rs ResetServiceSetting = ResetServiceSettingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResetServiceSettingResponse' Core.<$>
                   (x Core..:? "ServiceSetting") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result body of the ResetServiceSetting API action.
--
-- /See:/ 'mkResetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { serviceSetting :: Core.Maybe Types.ServiceSetting
    -- ^ The current, effective service setting after calling the ResetServiceSetting API action.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResetServiceSettingResponse' value with any optional fields omitted.
mkResetServiceSettingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetServiceSettingResponse
mkResetServiceSettingResponse responseStatus
  = ResetServiceSettingResponse'{serviceSetting = Core.Nothing,
                                 responseStatus}

-- | The current, effective service setting after calling the ResetServiceSetting API action.
--
-- /Note:/ Consider using 'serviceSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrrsServiceSetting :: Lens.Lens' ResetServiceSettingResponse (Core.Maybe Types.ServiceSetting)
rssrrsServiceSetting = Lens.field @"serviceSetting"
{-# INLINEABLE rssrrsServiceSetting #-}
{-# DEPRECATED serviceSetting "Use generic-lens or generic-optics with 'serviceSetting' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssrrsResponseStatus :: Lens.Lens' ResetServiceSettingResponse Core.Int
rssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
