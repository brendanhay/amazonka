-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
  ( AccountRecoverySettingType (..),

    -- * Smart constructor
    mkAccountRecoverySettingType,

    -- * Lenses
    arstRecoveryMechanisms,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The data type for @AccountRecoverySetting@ .
--
-- /See:/ 'mkAccountRecoverySettingType' smart constructor.
newtype AccountRecoverySettingType = AccountRecoverySettingType'
  { recoveryMechanisms ::
      Lude.Maybe
        ( Lude.NonEmpty
            RecoveryOptionType
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountRecoverySettingType' with the minimum fields required to make a request.
--
-- * 'recoveryMechanisms' - The list of @RecoveryOptionTypes@ .
mkAccountRecoverySettingType ::
  AccountRecoverySettingType
mkAccountRecoverySettingType =
  AccountRecoverySettingType' {recoveryMechanisms = Lude.Nothing}

-- | The list of @RecoveryOptionTypes@ .
--
-- /Note:/ Consider using 'recoveryMechanisms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arstRecoveryMechanisms :: Lens.Lens' AccountRecoverySettingType (Lude.Maybe (Lude.NonEmpty RecoveryOptionType))
arstRecoveryMechanisms = Lens.lens (recoveryMechanisms :: AccountRecoverySettingType -> Lude.Maybe (Lude.NonEmpty RecoveryOptionType)) (\s a -> s {recoveryMechanisms = a} :: AccountRecoverySettingType)
{-# DEPRECATED arstRecoveryMechanisms "Use generic-lens or generic-optics with 'recoveryMechanisms' instead." #-}

instance Lude.FromJSON AccountRecoverySettingType where
  parseJSON =
    Lude.withObject
      "AccountRecoverySettingType"
      ( \x ->
          AccountRecoverySettingType'
            Lude.<$> (x Lude..:? "RecoveryMechanisms")
      )

instance Lude.ToJSON AccountRecoverySettingType where
  toJSON AccountRecoverySettingType' {..} =
    Lude.object
      ( Lude.catMaybes
          [("RecoveryMechanisms" Lude..=) Lude.<$> recoveryMechanisms]
      )
