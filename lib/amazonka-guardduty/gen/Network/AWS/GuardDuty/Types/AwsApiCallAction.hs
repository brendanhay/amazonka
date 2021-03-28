{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AwsApiCallAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.AwsApiCallAction
  ( AwsApiCallAction (..)
  -- * Smart constructor
  , mkAwsApiCallAction
  -- * Lenses
  , aacaApi
  , aacaCallerType
  , aacaDomainDetails
  , aacaErrorCode
  , aacaRemoteIpDetails
  , aacaServiceName
  ) where

import qualified Network.AWS.GuardDuty.Types.DomainDetails as Types
import qualified Network.AWS.GuardDuty.Types.RemoteIpDetails as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the API action.
--
-- /See:/ 'mkAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { api :: Core.Maybe Core.Text
    -- ^ The AWS API name.
  , callerType :: Core.Maybe Core.Text
    -- ^ The AWS API caller type.
  , domainDetails :: Core.Maybe Types.DomainDetails
    -- ^ The domain information for the AWS API call.
  , errorCode :: Core.Maybe Core.Text
    -- ^ The error code of the failed AWS API action.
  , remoteIpDetails :: Core.Maybe Types.RemoteIpDetails
    -- ^ The remote IP information of the connection that initiated the AWS API call.
  , serviceName :: Core.Maybe Core.Text
    -- ^ The AWS service name whose API was invoked.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AwsApiCallAction' value with any optional fields omitted.
mkAwsApiCallAction
    :: AwsApiCallAction
mkAwsApiCallAction
  = AwsApiCallAction'{api = Core.Nothing, callerType = Core.Nothing,
                      domainDetails = Core.Nothing, errorCode = Core.Nothing,
                      remoteIpDetails = Core.Nothing, serviceName = Core.Nothing}

-- | The AWS API name.
--
-- /Note:/ Consider using 'api' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaApi :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
aacaApi = Lens.field @"api"
{-# INLINEABLE aacaApi #-}
{-# DEPRECATED api "Use generic-lens or generic-optics with 'api' instead"  #-}

-- | The AWS API caller type.
--
-- /Note:/ Consider using 'callerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaCallerType :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
aacaCallerType = Lens.field @"callerType"
{-# INLINEABLE aacaCallerType #-}
{-# DEPRECATED callerType "Use generic-lens or generic-optics with 'callerType' instead"  #-}

-- | The domain information for the AWS API call.
--
-- /Note:/ Consider using 'domainDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaDomainDetails :: Lens.Lens' AwsApiCallAction (Core.Maybe Types.DomainDetails)
aacaDomainDetails = Lens.field @"domainDetails"
{-# INLINEABLE aacaDomainDetails #-}
{-# DEPRECATED domainDetails "Use generic-lens or generic-optics with 'domainDetails' instead"  #-}

-- | The error code of the failed AWS API action.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaErrorCode :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
aacaErrorCode = Lens.field @"errorCode"
{-# INLINEABLE aacaErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The remote IP information of the connection that initiated the AWS API call.
--
-- /Note:/ Consider using 'remoteIpDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaRemoteIpDetails :: Lens.Lens' AwsApiCallAction (Core.Maybe Types.RemoteIpDetails)
aacaRemoteIpDetails = Lens.field @"remoteIpDetails"
{-# INLINEABLE aacaRemoteIpDetails #-}
{-# DEPRECATED remoteIpDetails "Use generic-lens or generic-optics with 'remoteIpDetails' instead"  #-}

-- | The AWS service name whose API was invoked.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacaServiceName :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
aacaServiceName = Lens.field @"serviceName"
{-# INLINEABLE aacaServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

instance Core.FromJSON AwsApiCallAction where
        parseJSON
          = Core.withObject "AwsApiCallAction" Core.$
              \ x ->
                AwsApiCallAction' Core.<$>
                  (x Core..:? "api") Core.<*> x Core..:? "callerType" Core.<*>
                    x Core..:? "domainDetails"
                    Core.<*> x Core..:? "errorCode"
                    Core.<*> x Core..:? "remoteIpDetails"
                    Core.<*> x Core..:? "serviceName"
