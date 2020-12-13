{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AccountModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AccountModification
  ( AccountModification (..),

    -- * Smart constructor
    mkAccountModification,

    -- * Lenses
    amStartTime,
    amDedicatedTenancySupport,
    amModificationState,
    amDedicatedTenancyManagementCidrRange,
    amErrorCode,
    amErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum

-- | Describes a modification to the configuration of Bring Your Own License (BYOL) for the specified account.
--
-- /See:/ 'mkAccountModification' smart constructor.
data AccountModification = AccountModification'
  { -- | The timestamp when the modification of the BYOL configuration was started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of BYOL (whether BYOL is being enabled or disabled).
    dedicatedTenancySupport :: Lude.Maybe DedicatedTenancySupportResultEnum,
    -- | The state of the modification to the configuration of BYOL.
    modificationState :: Lude.Maybe DedicatedTenancyModificationStateEnum,
    -- | The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
    dedicatedTenancyManagementCidrRange :: Lude.Maybe Lude.Text,
    -- | The error code that is returned if the configuration of BYOL cannot be modified.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The text of the error message that is returned if the configuration of BYOL cannot be modified.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountModification' with the minimum fields required to make a request.
--
-- * 'startTime' - The timestamp when the modification of the BYOL configuration was started.
-- * 'dedicatedTenancySupport' - The status of BYOL (whether BYOL is being enabled or disabled).
-- * 'modificationState' - The state of the modification to the configuration of BYOL.
-- * 'dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
-- * 'errorCode' - The error code that is returned if the configuration of BYOL cannot be modified.
-- * 'errorMessage' - The text of the error message that is returned if the configuration of BYOL cannot be modified.
mkAccountModification ::
  AccountModification
mkAccountModification =
  AccountModification'
    { startTime = Lude.Nothing,
      dedicatedTenancySupport = Lude.Nothing,
      modificationState = Lude.Nothing,
      dedicatedTenancyManagementCidrRange = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The timestamp when the modification of the BYOL configuration was started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amStartTime :: Lens.Lens' AccountModification (Lude.Maybe Lude.Timestamp)
amStartTime = Lens.lens (startTime :: AccountModification -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: AccountModification)
{-# DEPRECATED amStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of BYOL (whether BYOL is being enabled or disabled).
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDedicatedTenancySupport :: Lens.Lens' AccountModification (Lude.Maybe DedicatedTenancySupportResultEnum)
amDedicatedTenancySupport = Lens.lens (dedicatedTenancySupport :: AccountModification -> Lude.Maybe DedicatedTenancySupportResultEnum) (\s a -> s {dedicatedTenancySupport = a} :: AccountModification)
{-# DEPRECATED amDedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead." #-}

-- | The state of the modification to the configuration of BYOL.
--
-- /Note:/ Consider using 'modificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amModificationState :: Lens.Lens' AccountModification (Lude.Maybe DedicatedTenancyModificationStateEnum)
amModificationState = Lens.lens (modificationState :: AccountModification -> Lude.Maybe DedicatedTenancyModificationStateEnum) (\s a -> s {modificationState = a} :: AccountModification)
{-# DEPRECATED amModificationState "Use generic-lens or generic-optics with 'modificationState' instead." #-}

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDedicatedTenancyManagementCidrRange :: Lens.Lens' AccountModification (Lude.Maybe Lude.Text)
amDedicatedTenancyManagementCidrRange = Lens.lens (dedicatedTenancyManagementCidrRange :: AccountModification -> Lude.Maybe Lude.Text) (\s a -> s {dedicatedTenancyManagementCidrRange = a} :: AccountModification)
{-# DEPRECATED amDedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead." #-}

-- | The error code that is returned if the configuration of BYOL cannot be modified.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amErrorCode :: Lens.Lens' AccountModification (Lude.Maybe Lude.Text)
amErrorCode = Lens.lens (errorCode :: AccountModification -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: AccountModification)
{-# DEPRECATED amErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The text of the error message that is returned if the configuration of BYOL cannot be modified.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amErrorMessage :: Lens.Lens' AccountModification (Lude.Maybe Lude.Text)
amErrorMessage = Lens.lens (errorMessage :: AccountModification -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: AccountModification)
{-# DEPRECATED amErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON AccountModification where
  parseJSON =
    Lude.withObject
      "AccountModification"
      ( \x ->
          AccountModification'
            Lude.<$> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "DedicatedTenancySupport")
            Lude.<*> (x Lude..:? "ModificationState")
            Lude.<*> (x Lude..:? "DedicatedTenancyManagementCidrRange")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
