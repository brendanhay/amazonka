{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.AccountSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.AccountSettings
  ( AccountSettings (..),

    -- * Smart constructor
    mkAccountSettings,

    -- * Lenses
    asSkipAppResign,
    asAwsAccountNumber,
    asMaxJobTimeoutMinutes,
    asMaxSlots,
    asTrialMinutes,
    asUnmeteredDevices,
    asUnmeteredRemoteAccessDevices,
    asDefaultJobTimeoutMinutes,
  )
where

import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.TrialMinutes
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for account-level settings in AWS Device Farm.
--
-- /See:/ 'mkAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { skipAppResign ::
      Lude.Maybe Lude.Bool,
    awsAccountNumber :: Lude.Maybe Lude.Text,
    maxJobTimeoutMinutes :: Lude.Maybe Lude.Int,
    maxSlots :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)),
    trialMinutes :: Lude.Maybe TrialMinutes,
    unmeteredDevices ::
      Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int)),
    unmeteredRemoteAccessDevices ::
      Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int)),
    defaultJobTimeoutMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountSettings' with the minimum fields required to make a request.
--
-- * 'awsAccountNumber' - The AWS account number specified in the @AccountSettings@ container.
-- * 'defaultJobTimeoutMinutes' - The default number of minutes (at the account level) a test run executes before it times out. The default value is 150 minutes.
-- * 'maxJobTimeoutMinutes' - The maximum number of minutes a test run executes before it times out.
-- * 'maxSlots' - The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
-- * 'skipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
-- * 'trialMinutes' - Information about an AWS account's usage of free trial device minutes.
-- * 'unmeteredDevices' - Returns the unmetered devices you have purchased or want to purchase.
-- * 'unmeteredRemoteAccessDevices' - Returns the unmetered remote access devices you have purchased or want to purchase.
mkAccountSettings ::
  AccountSettings
mkAccountSettings =
  AccountSettings'
    { skipAppResign = Lude.Nothing,
      awsAccountNumber = Lude.Nothing,
      maxJobTimeoutMinutes = Lude.Nothing,
      maxSlots = Lude.Nothing,
      trialMinutes = Lude.Nothing,
      unmeteredDevices = Lude.Nothing,
      unmeteredRemoteAccessDevices = Lude.Nothing,
      defaultJobTimeoutMinutes = Lude.Nothing
    }

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSkipAppResign :: Lens.Lens' AccountSettings (Lude.Maybe Lude.Bool)
asSkipAppResign = Lens.lens (skipAppResign :: AccountSettings -> Lude.Maybe Lude.Bool) (\s a -> s {skipAppResign = a} :: AccountSettings)
{-# DEPRECATED asSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | The AWS account number specified in the @AccountSettings@ container.
--
-- /Note:/ Consider using 'awsAccountNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAwsAccountNumber :: Lens.Lens' AccountSettings (Lude.Maybe Lude.Text)
asAwsAccountNumber = Lens.lens (awsAccountNumber :: AccountSettings -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountNumber = a} :: AccountSettings)
{-# DEPRECATED asAwsAccountNumber "Use generic-lens or generic-optics with 'awsAccountNumber' instead." #-}

-- | The maximum number of minutes a test run executes before it times out.
--
-- /Note:/ Consider using 'maxJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxJobTimeoutMinutes :: Lens.Lens' AccountSettings (Lude.Maybe Lude.Int)
asMaxJobTimeoutMinutes = Lens.lens (maxJobTimeoutMinutes :: AccountSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxJobTimeoutMinutes = a} :: AccountSettings)
{-# DEPRECATED asMaxJobTimeoutMinutes "Use generic-lens or generic-optics with 'maxJobTimeoutMinutes' instead." #-}

-- | The maximum number of device slots that the AWS account can purchase. Each maximum is expressed as an @offering-id:number@ pair, where the @offering-id@ represents one of the IDs returned by the @ListOfferings@ command.
--
-- /Note:/ Consider using 'maxSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMaxSlots :: Lens.Lens' AccountSettings (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
asMaxSlots = Lens.lens (maxSlots :: AccountSettings -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {maxSlots = a} :: AccountSettings)
{-# DEPRECATED asMaxSlots "Use generic-lens or generic-optics with 'maxSlots' instead." #-}

-- | Information about an AWS account's usage of free trial device minutes.
--
-- /Note:/ Consider using 'trialMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTrialMinutes :: Lens.Lens' AccountSettings (Lude.Maybe TrialMinutes)
asTrialMinutes = Lens.lens (trialMinutes :: AccountSettings -> Lude.Maybe TrialMinutes) (\s a -> s {trialMinutes = a} :: AccountSettings)
{-# DEPRECATED asTrialMinutes "Use generic-lens or generic-optics with 'trialMinutes' instead." #-}

-- | Returns the unmetered devices you have purchased or want to purchase.
--
-- /Note:/ Consider using 'unmeteredDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asUnmeteredDevices :: Lens.Lens' AccountSettings (Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int)))
asUnmeteredDevices = Lens.lens (unmeteredDevices :: AccountSettings -> Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int))) (\s a -> s {unmeteredDevices = a} :: AccountSettings)
{-# DEPRECATED asUnmeteredDevices "Use generic-lens or generic-optics with 'unmeteredDevices' instead." #-}

-- | Returns the unmetered remote access devices you have purchased or want to purchase.
--
-- /Note:/ Consider using 'unmeteredRemoteAccessDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asUnmeteredRemoteAccessDevices :: Lens.Lens' AccountSettings (Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int)))
asUnmeteredRemoteAccessDevices = Lens.lens (unmeteredRemoteAccessDevices :: AccountSettings -> Lude.Maybe (Lude.HashMap DevicePlatform (Lude.Int))) (\s a -> s {unmeteredRemoteAccessDevices = a} :: AccountSettings)
{-# DEPRECATED asUnmeteredRemoteAccessDevices "Use generic-lens or generic-optics with 'unmeteredRemoteAccessDevices' instead." #-}

-- | The default number of minutes (at the account level) a test run executes before it times out. The default value is 150 minutes.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDefaultJobTimeoutMinutes :: Lens.Lens' AccountSettings (Lude.Maybe Lude.Int)
asDefaultJobTimeoutMinutes = Lens.lens (defaultJobTimeoutMinutes :: AccountSettings -> Lude.Maybe Lude.Int) (\s a -> s {defaultJobTimeoutMinutes = a} :: AccountSettings)
{-# DEPRECATED asDefaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead." #-}

instance Lude.FromJSON AccountSettings where
  parseJSON =
    Lude.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Lude.<$> (x Lude..:? "skipAppResign")
            Lude.<*> (x Lude..:? "awsAccountNumber")
            Lude.<*> (x Lude..:? "maxJobTimeoutMinutes")
            Lude.<*> (x Lude..:? "maxSlots" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "trialMinutes")
            Lude.<*> (x Lude..:? "unmeteredDevices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "unmeteredRemoteAccessDevices" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "defaultJobTimeoutMinutes")
      )
