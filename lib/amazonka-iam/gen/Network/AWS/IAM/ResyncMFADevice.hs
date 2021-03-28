{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with its IAM resource object on the AWS servers.
--
-- For more information about creating and working with virtual MFA devices, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /IAM User Guide/ .
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Creating a request
      ResyncMFADevice (..)
    , mkResyncMFADevice
    -- ** Request lenses
    , rmfadUserName
    , rmfadSerialNumber
    , rmfadAuthenticationCode1
    , rmfadAuthenticationCode2

    -- * Destructuring the response
    , ResyncMFADeviceResponse (..)
    , mkResyncMFADeviceResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResyncMFADevice' smart constructor.
data ResyncMFADevice = ResyncMFADevice'
  { userName :: Types.UserName
    -- ^ The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , serialNumber :: Types.SerialNumberType
    -- ^ Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , authenticationCode1 :: Types.AuthenticationCodeType
    -- ^ An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
  , authenticationCode2 :: Types.AuthenticationCodeType
    -- ^ A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResyncMFADevice' value with any optional fields omitted.
mkResyncMFADevice
    :: Types.UserName -- ^ 'userName'
    -> Types.SerialNumberType -- ^ 'serialNumber'
    -> Types.AuthenticationCodeType -- ^ 'authenticationCode1'
    -> Types.AuthenticationCodeType -- ^ 'authenticationCode2'
    -> ResyncMFADevice
mkResyncMFADevice userName serialNumber authenticationCode1
  authenticationCode2
  = ResyncMFADevice'{userName, serialNumber, authenticationCode1,
                     authenticationCode2}

-- | The name of the user whose MFA device you want to resynchronize.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmfadUserName :: Lens.Lens' ResyncMFADevice Types.UserName
rmfadUserName = Lens.field @"userName"
{-# INLINEABLE rmfadUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | Serial number that uniquely identifies the MFA device.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmfadSerialNumber :: Lens.Lens' ResyncMFADevice Types.SerialNumberType
rmfadSerialNumber = Lens.field @"serialNumber"
{-# INLINEABLE rmfadSerialNumber #-}
{-# DEPRECATED serialNumber "Use generic-lens or generic-optics with 'serialNumber' instead"  #-}

-- | An authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
--
-- /Note:/ Consider using 'authenticationCode1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmfadAuthenticationCode1 :: Lens.Lens' ResyncMFADevice Types.AuthenticationCodeType
rmfadAuthenticationCode1 = Lens.field @"authenticationCode1"
{-# INLINEABLE rmfadAuthenticationCode1 #-}
{-# DEPRECATED authenticationCode1 "Use generic-lens or generic-optics with 'authenticationCode1' instead"  #-}

-- | A subsequent authentication code emitted by the device.
--
-- The format for this parameter is a sequence of six digits.
--
-- /Note:/ Consider using 'authenticationCode2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmfadAuthenticationCode2 :: Lens.Lens' ResyncMFADevice Types.AuthenticationCodeType
rmfadAuthenticationCode2 = Lens.field @"authenticationCode2"
{-# INLINEABLE rmfadAuthenticationCode2 #-}
{-# DEPRECATED authenticationCode2 "Use generic-lens or generic-optics with 'authenticationCode2' instead"  #-}

instance Core.ToQuery ResyncMFADevice where
        toQuery ResyncMFADevice{..}
          = Core.toQueryPair "Action" ("ResyncMFADevice" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "UserName" userName
              Core.<> Core.toQueryPair "SerialNumber" serialNumber
              Core.<> Core.toQueryPair "AuthenticationCode1" authenticationCode1
              Core.<> Core.toQueryPair "AuthenticationCode2" authenticationCode2

instance Core.ToHeaders ResyncMFADevice where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResyncMFADevice where
        type Rs ResyncMFADevice = ResyncMFADeviceResponse
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
        parseResponse = Response.receiveNull ResyncMFADeviceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse = ResyncMFADeviceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResyncMFADeviceResponse' value with any optional fields omitted.
mkResyncMFADeviceResponse
    :: ResyncMFADeviceResponse
mkResyncMFADeviceResponse = ResyncMFADeviceResponse'
