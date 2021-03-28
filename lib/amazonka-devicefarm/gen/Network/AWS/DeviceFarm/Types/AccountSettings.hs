{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.AccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.AccountSettings
  ( AccountSettings (..)
  -- * Smart constructor
  , mkAccountSettings
  -- * Lenses
  , asAwsAccountNumber
  , asDefaultJobTimeoutMinutes
  , asMaxJobTimeoutMinutes
  , asMaxSlots
  , asSkipAppResign
  , asTrialMinutes
  , asUnmeteredDevices
  , asUnmeteredRemoteAccessDevices
  ) where

import qualified Network.AWS.DeviceFarm.Types.AwsAccountNumber as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePlatform as Types
import qualified Network.AWS.DeviceFarm.Types.TrialMinutes as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for account-level settings in AWS Device Farm.
--
-- /See:/ 'mkAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { awsAccountNumber :: Core.Maybe Types.AwsAccountNumber
    -- ^ The AWS account number specified in the @AccountSettings@ container.
  , defaultJobTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ The default number of minutes (at the account level) a test run executes before it times out. The default value is 150 minutes.
  , maxJobTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ The maximum number of minutes a test run executes before it times out.
  , maxSlots :: Core.Maybe (Core.HashMap Core.Text Core.Int)
    -- ^ The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
  , skipAppResign :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
  , trialMinutes :: Core.Maybe Types.TrialMinutes
    -- ^ Information about an AWS account's usage of free trial device minutes.
  , unmeteredDevices :: Core.Maybe (Core.HashMap Types.DevicePlatform Core.Int)
    -- ^ Returns the unmetered devices you have purchased or want to purchase.
  , unmeteredRemoteAccessDevices :: Core.Maybe (Core.HashMap Types.DevicePlatform Core.Int)
    -- ^ Returns the unmetered remote access devices you have purchased or want to purchase.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountSettings' value with any optional fields omitted.
mkAccountSettings
    :: AccountSettings
mkAccountSettings
  = AccountSettings'{awsAccountNumber = Core.Nothing,
                     defaultJobTimeoutMinutes = Core.Nothing,
                     maxJobTimeoutMinutes = Core.Nothing, maxSlots = Core.Nothing,
                     skipAppResign = Core.Nothing, trialMinutes = Core.Nothing,
                     unmeteredDevices = Core.Nothing,
                     unmeteredRemoteAccessDevices = Core.Nothing}

-- | The AWS account number specified in the @AccountSettings@ container.
--
-- /Note:/ Consider using 'awsAccountNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAwsAccountNumber :: Lens.Lens' AccountSettings (Core.Maybe Types.AwsAccountNumber)
asAwsAccountNumber = Lens.field @"awsAccountNumber"
{-# INLINEABLE asAwsAccountNumber #-}
{-# DEPRECATED awsAccountNumber "Use generic-lens or generic-optics with 'awsAccountNumber' instead"  #-}

-- | The default number of minutes (at the account level) a test run executes before it times out. The default value is 150 minutes.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDefaultJobTimeoutMinutes :: Lens.Lens' AccountSettings (Core.Maybe Core.Int)
asDefaultJobTimeoutMinutes = Lens.field @"defaultJobTimeoutMinutes"
{-# INLINEABLE asDefaultJobTimeoutMinutes #-}
{-# DEPRECATED defaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead"  #-}

-- | The maximum number of minutes a test run executes before it times out.
--
-- /Note:/ Consider using 'maxJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxJobTimeoutMinutes :: Lens.Lens' AccountSettings (Core.Maybe Core.Int)
asMaxJobTimeoutMinutes = Lens.field @"maxJobTimeoutMinutes"
{-# INLINEABLE asMaxJobTimeoutMinutes #-}
{-# DEPRECATED maxJobTimeoutMinutes "Use generic-lens or generic-optics with 'maxJobTimeoutMinutes' instead"  #-}

-- | The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
--
-- /Note:/ Consider using 'maxSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxSlots :: Lens.Lens' AccountSettings (Core.Maybe (Core.HashMap Core.Text Core.Int))
asMaxSlots = Lens.field @"maxSlots"
{-# INLINEABLE asMaxSlots #-}
{-# DEPRECATED maxSlots "Use generic-lens or generic-optics with 'maxSlots' instead"  #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSkipAppResign :: Lens.Lens' AccountSettings (Core.Maybe Core.Bool)
asSkipAppResign = Lens.field @"skipAppResign"
{-# INLINEABLE asSkipAppResign #-}
{-# DEPRECATED skipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead"  #-}

-- | Information about an AWS account's usage of free trial device minutes.
--
-- /Note:/ Consider using 'trialMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTrialMinutes :: Lens.Lens' AccountSettings (Core.Maybe Types.TrialMinutes)
asTrialMinutes = Lens.field @"trialMinutes"
{-# INLINEABLE asTrialMinutes #-}
{-# DEPRECATED trialMinutes "Use generic-lens or generic-optics with 'trialMinutes' instead"  #-}

-- | Returns the unmetered devices you have purchased or want to purchase.
--
-- /Note:/ Consider using 'unmeteredDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asUnmeteredDevices :: Lens.Lens' AccountSettings (Core.Maybe (Core.HashMap Types.DevicePlatform Core.Int))
asUnmeteredDevices = Lens.field @"unmeteredDevices"
{-# INLINEABLE asUnmeteredDevices #-}
{-# DEPRECATED unmeteredDevices "Use generic-lens or generic-optics with 'unmeteredDevices' instead"  #-}

-- | Returns the unmetered remote access devices you have purchased or want to purchase.
--
-- /Note:/ Consider using 'unmeteredRemoteAccessDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asUnmeteredRemoteAccessDevices :: Lens.Lens' AccountSettings (Core.Maybe (Core.HashMap Types.DevicePlatform Core.Int))
asUnmeteredRemoteAccessDevices = Lens.field @"unmeteredRemoteAccessDevices"
{-# INLINEABLE asUnmeteredRemoteAccessDevices #-}
{-# DEPRECATED unmeteredRemoteAccessDevices "Use generic-lens or generic-optics with 'unmeteredRemoteAccessDevices' instead"  #-}

instance Core.FromJSON AccountSettings where
        parseJSON
          = Core.withObject "AccountSettings" Core.$
              \ x ->
                AccountSettings' Core.<$>
                  (x Core..:? "awsAccountNumber") Core.<*>
                    x Core..:? "defaultJobTimeoutMinutes"
                    Core.<*> x Core..:? "maxJobTimeoutMinutes"
                    Core.<*> x Core..:? "maxSlots"
                    Core.<*> x Core..:? "skipAppResign"
                    Core.<*> x Core..:? "trialMinutes"
                    Core.<*> x Core..:? "unmeteredDevices"
                    Core.<*> x Core..:? "unmeteredRemoteAccessDevices"
