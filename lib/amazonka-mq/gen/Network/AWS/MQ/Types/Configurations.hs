{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Configurations
  ( Configurations (..),

    -- * Smart constructor
    mkConfigurations,

    -- * Lenses
    cPending,
    cHistory,
    cCurrent,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ConfigurationId
import qualified Network.AWS.Prelude as Lude

-- | Broker configuration information
--
-- /See:/ 'mkConfigurations' smart constructor.
data Configurations = Configurations'
  { pending ::
      Lude.Maybe ConfigurationId,
    history :: Lude.Maybe [ConfigurationId],
    current :: Lude.Maybe ConfigurationId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Configurations' with the minimum fields required to make a request.
--
-- * 'current' - The current configuration of the broker.
-- * 'history' - The history of configurations applied to the broker.
-- * 'pending' - The pending configuration of the broker.
mkConfigurations ::
  Configurations
mkConfigurations =
  Configurations'
    { pending = Lude.Nothing,
      history = Lude.Nothing,
      current = Lude.Nothing
    }

-- | The pending configuration of the broker.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPending :: Lens.Lens' Configurations (Lude.Maybe ConfigurationId)
cPending = Lens.lens (pending :: Configurations -> Lude.Maybe ConfigurationId) (\s a -> s {pending = a} :: Configurations)
{-# DEPRECATED cPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | The history of configurations applied to the broker.
--
-- /Note:/ Consider using 'history' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHistory :: Lens.Lens' Configurations (Lude.Maybe [ConfigurationId])
cHistory = Lens.lens (history :: Configurations -> Lude.Maybe [ConfigurationId]) (\s a -> s {history = a} :: Configurations)
{-# DEPRECATED cHistory "Use generic-lens or generic-optics with 'history' instead." #-}

-- | The current configuration of the broker.
--
-- /Note:/ Consider using 'current' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCurrent :: Lens.Lens' Configurations (Lude.Maybe ConfigurationId)
cCurrent = Lens.lens (current :: Configurations -> Lude.Maybe ConfigurationId) (\s a -> s {current = a} :: Configurations)
{-# DEPRECATED cCurrent "Use generic-lens or generic-optics with 'current' instead." #-}

instance Lude.FromJSON Configurations where
  parseJSON =
    Lude.withObject
      "Configurations"
      ( \x ->
          Configurations'
            Lude.<$> (x Lude..:? "pending")
            Lude.<*> (x Lude..:? "history" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "current")
      )
