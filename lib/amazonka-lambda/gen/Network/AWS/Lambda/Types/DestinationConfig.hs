{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DestinationConfig
  ( DestinationConfig (..),

    -- * Smart constructor
    mkDestinationConfig,

    -- * Lenses
    dcOnSuccess,
    dcOnFailure,
  )
where

import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration object that specifies the destination of an event after Lambda processes it.
--
-- /See:/ 'mkDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { onSuccess ::
      Lude.Maybe OnSuccess,
    onFailure :: Lude.Maybe OnFailure
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationConfig' with the minimum fields required to make a request.
--
-- * 'onFailure' - The destination configuration for failed invocations.
-- * 'onSuccess' - The destination configuration for successful invocations.
mkDestinationConfig ::
  DestinationConfig
mkDestinationConfig =
  DestinationConfig'
    { onSuccess = Lude.Nothing,
      onFailure = Lude.Nothing
    }

-- | The destination configuration for successful invocations.
--
-- /Note:/ Consider using 'onSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOnSuccess :: Lens.Lens' DestinationConfig (Lude.Maybe OnSuccess)
dcOnSuccess = Lens.lens (onSuccess :: DestinationConfig -> Lude.Maybe OnSuccess) (\s a -> s {onSuccess = a} :: DestinationConfig)
{-# DEPRECATED dcOnSuccess "Use generic-lens or generic-optics with 'onSuccess' instead." #-}

-- | The destination configuration for failed invocations.
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOnFailure :: Lens.Lens' DestinationConfig (Lude.Maybe OnFailure)
dcOnFailure = Lens.lens (onFailure :: DestinationConfig -> Lude.Maybe OnFailure) (\s a -> s {onFailure = a} :: DestinationConfig)
{-# DEPRECATED dcOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

instance Lude.FromJSON DestinationConfig where
  parseJSON =
    Lude.withObject
      "DestinationConfig"
      ( \x ->
          DestinationConfig'
            Lude.<$> (x Lude..:? "OnSuccess") Lude.<*> (x Lude..:? "OnFailure")
      )

instance Lude.ToJSON DestinationConfig where
  toJSON DestinationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OnSuccess" Lude..=) Lude.<$> onSuccess,
            ("OnFailure" Lude..=) Lude.<$> onFailure
          ]
      )
