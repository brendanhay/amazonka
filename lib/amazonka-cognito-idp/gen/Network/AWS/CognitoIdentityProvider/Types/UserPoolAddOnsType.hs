{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
  ( UserPoolAddOnsType (..),

    -- * Smart constructor
    mkUserPoolAddOnsType,

    -- * Lenses
    upaotAdvancedSecurityMode,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The user pool add-ons type.
--
-- /See:/ 'mkUserPoolAddOnsType' smart constructor.
newtype UserPoolAddOnsType = UserPoolAddOnsType'
  { advancedSecurityMode ::
      AdvancedSecurityModeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolAddOnsType' with the minimum fields required to make a request.
--
-- * 'advancedSecurityMode' - The advanced security mode.
mkUserPoolAddOnsType ::
  -- | 'advancedSecurityMode'
  AdvancedSecurityModeType ->
  UserPoolAddOnsType
mkUserPoolAddOnsType pAdvancedSecurityMode_ =
  UserPoolAddOnsType'
    { advancedSecurityMode =
        pAdvancedSecurityMode_
    }

-- | The advanced security mode.
--
-- /Note:/ Consider using 'advancedSecurityMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaotAdvancedSecurityMode :: Lens.Lens' UserPoolAddOnsType AdvancedSecurityModeType
upaotAdvancedSecurityMode = Lens.lens (advancedSecurityMode :: UserPoolAddOnsType -> AdvancedSecurityModeType) (\s a -> s {advancedSecurityMode = a} :: UserPoolAddOnsType)
{-# DEPRECATED upaotAdvancedSecurityMode "Use generic-lens or generic-optics with 'advancedSecurityMode' instead." #-}

instance Lude.FromJSON UserPoolAddOnsType where
  parseJSON =
    Lude.withObject
      "UserPoolAddOnsType"
      ( \x ->
          UserPoolAddOnsType' Lude.<$> (x Lude..: "AdvancedSecurityMode")
      )

instance Lude.ToJSON UserPoolAddOnsType where
  toJSON UserPoolAddOnsType' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AdvancedSecurityMode" Lude..= advancedSecurityMode)]
      )
