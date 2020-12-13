{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
  ( RecoveryOptionType (..),

    -- * Smart constructor
    mkRecoveryOptionType,

    -- * Lenses
    rotPriority,
    rotName,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A map containing a priority as a key, and recovery method name as a value.
--
-- /See:/ 'mkRecoveryOptionType' smart constructor.
data RecoveryOptionType = RecoveryOptionType'
  { -- | A positive integer specifying priority of a method with 1 being the highest priority.
    priority :: Lude.Natural,
    -- | Specifies the recovery method for a user.
    name :: RecoveryOptionNameType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecoveryOptionType' with the minimum fields required to make a request.
--
-- * 'priority' - A positive integer specifying priority of a method with 1 being the highest priority.
-- * 'name' - Specifies the recovery method for a user.
mkRecoveryOptionType ::
  -- | 'priority'
  Lude.Natural ->
  -- | 'name'
  RecoveryOptionNameType ->
  RecoveryOptionType
mkRecoveryOptionType pPriority_ pName_ =
  RecoveryOptionType' {priority = pPriority_, name = pName_}

-- | A positive integer specifying priority of a method with 1 being the highest priority.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rotPriority :: Lens.Lens' RecoveryOptionType Lude.Natural
rotPriority = Lens.lens (priority :: RecoveryOptionType -> Lude.Natural) (\s a -> s {priority = a} :: RecoveryOptionType)
{-# DEPRECATED rotPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specifies the recovery method for a user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rotName :: Lens.Lens' RecoveryOptionType RecoveryOptionNameType
rotName = Lens.lens (name :: RecoveryOptionType -> RecoveryOptionNameType) (\s a -> s {name = a} :: RecoveryOptionType)
{-# DEPRECATED rotName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON RecoveryOptionType where
  parseJSON =
    Lude.withObject
      "RecoveryOptionType"
      ( \x ->
          RecoveryOptionType'
            Lude.<$> (x Lude..: "Priority") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON RecoveryOptionType where
  toJSON RecoveryOptionType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Priority" Lude..= priority),
            Lude.Just ("Name" Lude..= name)
          ]
      )
