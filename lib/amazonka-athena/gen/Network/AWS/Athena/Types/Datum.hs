{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Datum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Datum
  ( Datum (..),

    -- * Smart constructor
    mkDatum,

    -- * Lenses
    dVarCharValue,
  )
where

import qualified Network.AWS.Athena.Types.DatumString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A piece of data (a field in the table).
--
-- /See:/ 'mkDatum' smart constructor.
newtype Datum = Datum'
  { -- | The value of the datum.
    varCharValue :: Core.Maybe Types.DatumString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Datum' value with any optional fields omitted.
mkDatum ::
  Datum
mkDatum = Datum' {varCharValue = Core.Nothing}

-- | The value of the datum.
--
-- /Note:/ Consider using 'varCharValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVarCharValue :: Lens.Lens' Datum (Core.Maybe Types.DatumString)
dVarCharValue = Lens.field @"varCharValue"
{-# DEPRECATED dVarCharValue "Use generic-lens or generic-optics with 'varCharValue' instead." #-}

instance Core.FromJSON Datum where
  parseJSON =
    Core.withObject "Datum" Core.$
      \x -> Datum' Core.<$> (x Core..:? "VarCharValue")
