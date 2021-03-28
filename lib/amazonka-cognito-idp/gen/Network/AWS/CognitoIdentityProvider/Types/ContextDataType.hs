{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ContextDataType
  ( ContextDataType (..)
  -- * Smart constructor
  , mkContextDataType
  -- * Lenses
  , cdtIpAddress
  , cdtServerName
  , cdtServerPath
  , cdtHttpHeaders
  , cdtEncodedData
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.EncodedData as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.HttpHeader as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.IpAddress as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ServerName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ServerPath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contextual user data type used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- /See:/ 'mkContextDataType' smart constructor.
data ContextDataType = ContextDataType'
  { ipAddress :: Types.IpAddress
    -- ^ Source IP address of your user.
  , serverName :: Types.ServerName
    -- ^ Your server endpoint where this API is invoked.
  , serverPath :: Types.ServerPath
    -- ^ Your server path where this API is invoked. 
  , httpHeaders :: [Types.HttpHeader]
    -- ^ HttpHeaders received on your server in same order.
  , encodedData :: Core.Maybe Types.EncodedData
    -- ^ Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContextDataType' value with any optional fields omitted.
mkContextDataType
    :: Types.IpAddress -- ^ 'ipAddress'
    -> Types.ServerName -- ^ 'serverName'
    -> Types.ServerPath -- ^ 'serverPath'
    -> ContextDataType
mkContextDataType ipAddress serverName serverPath
  = ContextDataType'{ipAddress, serverName, serverPath,
                     httpHeaders = Core.mempty, encodedData = Core.Nothing}

-- | Source IP address of your user.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtIpAddress :: Lens.Lens' ContextDataType Types.IpAddress
cdtIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE cdtIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | Your server endpoint where this API is invoked.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtServerName :: Lens.Lens' ContextDataType Types.ServerName
cdtServerName = Lens.field @"serverName"
{-# INLINEABLE cdtServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Your server path where this API is invoked. 
--
-- /Note:/ Consider using 'serverPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtServerPath :: Lens.Lens' ContextDataType Types.ServerPath
cdtServerPath = Lens.field @"serverPath"
{-# INLINEABLE cdtServerPath #-}
{-# DEPRECATED serverPath "Use generic-lens or generic-optics with 'serverPath' instead"  #-}

-- | HttpHeaders received on your server in same order.
--
-- /Note:/ Consider using 'httpHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtHttpHeaders :: Lens.Lens' ContextDataType [Types.HttpHeader]
cdtHttpHeaders = Lens.field @"httpHeaders"
{-# INLINEABLE cdtHttpHeaders #-}
{-# DEPRECATED httpHeaders "Use generic-lens or generic-optics with 'httpHeaders' instead"  #-}

-- | Encoded data containing device fingerprinting details, collected using the Amazon Cognito context data collection library.
--
-- /Note:/ Consider using 'encodedData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtEncodedData :: Lens.Lens' ContextDataType (Core.Maybe Types.EncodedData)
cdtEncodedData = Lens.field @"encodedData"
{-# INLINEABLE cdtEncodedData #-}
{-# DEPRECATED encodedData "Use generic-lens or generic-optics with 'encodedData' instead"  #-}

instance Core.FromJSON ContextDataType where
        toJSON ContextDataType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IpAddress" Core..= ipAddress),
                  Core.Just ("ServerName" Core..= serverName),
                  Core.Just ("ServerPath" Core..= serverPath),
                  Core.Just ("HttpHeaders" Core..= httpHeaders),
                  ("EncodedData" Core..=) Core.<$> encodedData])
