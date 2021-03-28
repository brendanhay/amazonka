{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ActivateGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the gateway you previously deployed on your host. In the activation process, you specify information such as the AWS Region that you want to use for storing snapshots or tapes, the time zone for scheduled snapshots the gateway snapshot schedule window, an activation key, and a name for your gateway. The activation process also associates your gateway with your account. For more information, see 'UpdateGatewayInformation' .
module Network.AWS.StorageGateway.ActivateGateway
    (
    -- * Creating a request
      ActivateGateway (..)
    , mkActivateGateway
    -- ** Request lenses
    , agActivationKey
    , agGatewayName
    , agGatewayTimezone
    , agGatewayRegion
    , agGatewayType
    , agMediumChangerType
    , agTags
    , agTapeDriveType

    -- * Destructuring the response
    , ActivateGatewayResponse (..)
    , mkActivateGatewayResponse
    -- ** Response lenses
    , agrrsGatewayARN
    , agrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'ActivateGatewayInput$ActivationKey' 
--
--
--     * 'ActivateGatewayInput$GatewayName' 
--
--
--     * 'ActivateGatewayInput$GatewayRegion' 
--
--
--     * 'ActivateGatewayInput$GatewayTimezone' 
--
--
--     * 'ActivateGatewayInput$GatewayType' 
--
--
--     * 'ActivateGatewayInput$MediumChangerType' 
--
--
--     * 'ActivateGatewayInput$TapeDriveType' 
--
--
--
-- /See:/ 'mkActivateGateway' smart constructor.
data ActivateGateway = ActivateGateway'
  { activationKey :: Types.ActivationKey
    -- ^ Your gateway activation key. You can obtain the activation key by sending an HTTP GET request with redirects enabled to the gateway IP address (port 80). The redirect URL returned in the response provides you the activation key for your gateway in the query string parameter @activationKey@ . It may also include other activation-related parameters, however, these are merely defaults -- the arguments you pass to the @ActivateGateway@ API call determine the actual configuration of your gateway.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key> in the /AWS Storage Gateway User Guide/ .
  , gatewayName :: Types.GatewayName
    -- ^ The name you configured for your gateway.
  , gatewayTimezone :: Types.GatewayTimezone
    -- ^ A value that indicates the time zone you want to set for the gateway. The time zone is of the format "GMT-hr:mm" or "GMT+hr:mm". For example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00 indicates the time is 2 hours ahead of GMT. The time zone is used, for example, for scheduling snapshots and your gateway's maintenance schedule.
  , gatewayRegion :: Types.RegionId
    -- ^ A value that indicates the AWS Region where you want to store your data. The gateway AWS Region specified must be the same AWS Region as the AWS Region in your @Host@ header in the request. For more information about available AWS Regions and endpoints for AWS Storage Gateway, see <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
--
-- Valid Values: See <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ . 
  , gatewayType :: Core.Maybe Types.GatewayType
    -- ^ A value that defines the type of gateway to activate. The type specified is critical to all later functions of the gateway and cannot be changed after activation. The default value is @CACHED@ .
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@ 
  , mediumChangerType :: Core.Maybe Types.MediumChangerType
    -- ^ The value that indicates the type of medium changer to use for tape gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@ 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that you can assign to the gateway. Each tag is a key-value pair.
  , tapeDriveType :: Core.Maybe Types.TapeDriveType
    -- ^ The value that indicates the type of tape drive to use for tape gateway. This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivateGateway' value with any optional fields omitted.
mkActivateGateway
    :: Types.ActivationKey -- ^ 'activationKey'
    -> Types.GatewayName -- ^ 'gatewayName'
    -> Types.GatewayTimezone -- ^ 'gatewayTimezone'
    -> Types.RegionId -- ^ 'gatewayRegion'
    -> ActivateGateway
mkActivateGateway activationKey gatewayName gatewayTimezone
  gatewayRegion
  = ActivateGateway'{activationKey, gatewayName, gatewayTimezone,
                     gatewayRegion, gatewayType = Core.Nothing,
                     mediumChangerType = Core.Nothing, tags = Core.Nothing,
                     tapeDriveType = Core.Nothing}

-- | Your gateway activation key. You can obtain the activation key by sending an HTTP GET request with redirects enabled to the gateway IP address (port 80). The redirect URL returned in the response provides you the activation key for your gateway in the query string parameter @activationKey@ . It may also include other activation-related parameters, however, these are merely defaults -- the arguments you pass to the @ActivateGateway@ API call determine the actual configuration of your gateway.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/get-activation-key.html Getting activation key> in the /AWS Storage Gateway User Guide/ .
--
-- /Note:/ Consider using 'activationKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agActivationKey :: Lens.Lens' ActivateGateway Types.ActivationKey
agActivationKey = Lens.field @"activationKey"
{-# INLINEABLE agActivationKey #-}
{-# DEPRECATED activationKey "Use generic-lens or generic-optics with 'activationKey' instead"  #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayName :: Lens.Lens' ActivateGateway Types.GatewayName
agGatewayName = Lens.field @"gatewayName"
{-# INLINEABLE agGatewayName #-}
{-# DEPRECATED gatewayName "Use generic-lens or generic-optics with 'gatewayName' instead"  #-}

-- | A value that indicates the time zone you want to set for the gateway. The time zone is of the format "GMT-hr:mm" or "GMT+hr:mm". For example, GMT-4:00 indicates the time is 4 hours behind GMT. GMT+2:00 indicates the time is 2 hours ahead of GMT. The time zone is used, for example, for scheduling snapshots and your gateway's maintenance schedule.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayTimezone :: Lens.Lens' ActivateGateway Types.GatewayTimezone
agGatewayTimezone = Lens.field @"gatewayTimezone"
{-# INLINEABLE agGatewayTimezone #-}
{-# DEPRECATED gatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead"  #-}

-- | A value that indicates the AWS Region where you want to store your data. The gateway AWS Region specified must be the same AWS Region as the AWS Region in your @Host@ header in the request. For more information about available AWS Regions and endpoints for AWS Storage Gateway, see <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ .
--
-- Valid Values: See <https://docs.aws.amazon.com/general/latest/gr/sg.html AWS Storage Gateway endpoints and quotas> in the /AWS General Reference/ . 
--
-- /Note:/ Consider using 'gatewayRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayRegion :: Lens.Lens' ActivateGateway Types.RegionId
agGatewayRegion = Lens.field @"gatewayRegion"
{-# INLINEABLE agGatewayRegion #-}
{-# DEPRECATED gatewayRegion "Use generic-lens or generic-optics with 'gatewayRegion' instead"  #-}

-- | A value that defines the type of gateway to activate. The type specified is critical to all later functions of the gateway and cannot be changed after activation. The default value is @CACHED@ .
--
-- Valid Values: @STORED@ | @CACHED@ | @VTL@ | @FILE_S3@ 
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agGatewayType :: Lens.Lens' ActivateGateway (Core.Maybe Types.GatewayType)
agGatewayType = Lens.field @"gatewayType"
{-# INLINEABLE agGatewayType #-}
{-# DEPRECATED gatewayType "Use generic-lens or generic-optics with 'gatewayType' instead"  #-}

-- | The value that indicates the type of medium changer to use for tape gateway. This field is optional.
--
-- Valid Values: @STK-L700@ | @AWS-Gateway-VTL@ | @IBM-03584L32-0402@ 
--
-- /Note:/ Consider using 'mediumChangerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agMediumChangerType :: Lens.Lens' ActivateGateway (Core.Maybe Types.MediumChangerType)
agMediumChangerType = Lens.field @"mediumChangerType"
{-# INLINEABLE agMediumChangerType #-}
{-# DEPRECATED mediumChangerType "Use generic-lens or generic-optics with 'mediumChangerType' instead"  #-}

-- | A list of up to 50 tags that you can assign to the gateway. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agTags :: Lens.Lens' ActivateGateway (Core.Maybe [Types.Tag])
agTags = Lens.field @"tags"
{-# INLINEABLE agTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The value that indicates the type of tape drive to use for tape gateway. This field is optional.
--
-- Valid Values: @IBM-ULT3580-TD5@ 
--
-- /Note:/ Consider using 'tapeDriveType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agTapeDriveType :: Lens.Lens' ActivateGateway (Core.Maybe Types.TapeDriveType)
agTapeDriveType = Lens.field @"tapeDriveType"
{-# INLINEABLE agTapeDriveType #-}
{-# DEPRECATED tapeDriveType "Use generic-lens or generic-optics with 'tapeDriveType' instead"  #-}

instance Core.ToQuery ActivateGateway where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ActivateGateway where
        toHeaders ActivateGateway{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.ActivateGateway")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ActivateGateway where
        toJSON ActivateGateway{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ActivationKey" Core..= activationKey),
                  Core.Just ("GatewayName" Core..= gatewayName),
                  Core.Just ("GatewayTimezone" Core..= gatewayTimezone),
                  Core.Just ("GatewayRegion" Core..= gatewayRegion),
                  ("GatewayType" Core..=) Core.<$> gatewayType,
                  ("MediumChangerType" Core..=) Core.<$> mediumChangerType,
                  ("Tags" Core..=) Core.<$> tags,
                  ("TapeDriveType" Core..=) Core.<$> tapeDriveType])

instance Core.AWSRequest ActivateGateway where
        type Rs ActivateGateway = ActivateGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ActivateGatewayResponse' Core.<$>
                   (x Core..:? "GatewayARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | AWS Storage Gateway returns the Amazon Resource Name (ARN) of the activated gateway. It is a string made of information such as your account, gateway name, and AWS Region. This ARN is used to reference the gateway in other API operations as well as resource-based authorization.
--
-- /See:/ 'mkActivateGatewayResponse' smart constructor.
data ActivateGatewayResponse = ActivateGatewayResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivateGatewayResponse' value with any optional fields omitted.
mkActivateGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ActivateGatewayResponse
mkActivateGatewayResponse responseStatus
  = ActivateGatewayResponse'{gatewayARN = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrrsGatewayARN :: Lens.Lens' ActivateGatewayResponse (Core.Maybe Types.GatewayARN)
agrrsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE agrrsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrrsResponseStatus :: Lens.Lens' ActivateGatewayResponse Core.Int
agrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE agrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
