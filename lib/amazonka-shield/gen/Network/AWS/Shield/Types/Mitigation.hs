{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Mitigation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Mitigation
  ( Mitigation (..),

    -- * Smart constructor
    mkMitigation,

    -- * Lenses
    mMitigationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The mitigation applied to a DDoS attack.
--
-- /See:/ 'mkMitigation' smart constructor.
newtype Mitigation = Mitigation'
  { -- | The name of the mitigation taken for this attack.
    mitigationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mitigation' with the minimum fields required to make a request.
--
-- * 'mitigationName' - The name of the mitigation taken for this attack.
mkMitigation ::
  Mitigation
mkMitigation = Mitigation' {mitigationName = Lude.Nothing}

-- | The name of the mitigation taken for this attack.
--
-- /Note:/ Consider using 'mitigationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMitigationName :: Lens.Lens' Mitigation (Lude.Maybe Lude.Text)
mMitigationName = Lens.lens (mitigationName :: Mitigation -> Lude.Maybe Lude.Text) (\s a -> s {mitigationName = a} :: Mitigation)
{-# DEPRECATED mMitigationName "Use generic-lens or generic-optics with 'mitigationName' instead." #-}

instance Lude.FromJSON Mitigation where
  parseJSON =
    Lude.withObject
      "Mitigation"
      (\x -> Mitigation' Lude.<$> (x Lude..:? "MitigationName"))
