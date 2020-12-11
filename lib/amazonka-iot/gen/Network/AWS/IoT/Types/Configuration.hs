-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Configuration
  ( Configuration (..),

    -- * Smart constructor
    mkConfiguration,

    -- * Lenses
    cEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration.
--
-- /See:/ 'mkConfiguration' smart constructor.
newtype Configuration = Configuration'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- * 'enabled' - True to enable the configuration.
mkConfiguration ::
  Configuration
mkConfiguration = Configuration' {enabled = Lude.Nothing}

-- | True to enable the configuration.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnabled :: Lens.Lens' Configuration (Lude.Maybe Lude.Bool)
cEnabled = Lens.lens (enabled :: Configuration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: Configuration)
{-# DEPRECATED cEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON Configuration where
  parseJSON =
    Lude.withObject
      "Configuration"
      (\x -> Configuration' Lude.<$> (x Lude..:? "Enabled"))

instance Lude.ToJSON Configuration where
  toJSON Configuration' {..} =
    Lude.object
      (Lude.catMaybes [("Enabled" Lude..=) Lude.<$> enabled])
