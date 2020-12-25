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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.String as Types

-- | The mitigation applied to a DDoS attack.
--
-- /See:/ 'mkMitigation' smart constructor.
newtype Mitigation = Mitigation'
  { -- | The name of the mitigation taken for this attack.
    mitigationName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Mitigation' value with any optional fields omitted.
mkMitigation ::
  Mitigation
mkMitigation = Mitigation' {mitigationName = Core.Nothing}

-- | The name of the mitigation taken for this attack.
--
-- /Note:/ Consider using 'mitigationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMitigationName :: Lens.Lens' Mitigation (Core.Maybe Types.String)
mMitigationName = Lens.field @"mitigationName"
{-# DEPRECATED mMitigationName "Use generic-lens or generic-optics with 'mitigationName' instead." #-}

instance Core.FromJSON Mitigation where
  parseJSON =
    Core.withObject "Mitigation" Core.$
      \x -> Mitigation' Core.<$> (x Core..:? "MitigationName")
