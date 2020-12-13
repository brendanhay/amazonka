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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A piece of data (a field in the table).
--
-- /See:/ 'mkDatum' smart constructor.
newtype Datum = Datum'
  { -- | The value of the datum.
    varCharValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Datum' with the minimum fields required to make a request.
--
-- * 'varCharValue' - The value of the datum.
mkDatum ::
  Datum
mkDatum = Datum' {varCharValue = Lude.Nothing}

-- | The value of the datum.
--
-- /Note:/ Consider using 'varCharValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVarCharValue :: Lens.Lens' Datum (Lude.Maybe Lude.Text)
dVarCharValue = Lens.lens (varCharValue :: Datum -> Lude.Maybe Lude.Text) (\s a -> s {varCharValue = a} :: Datum)
{-# DEPRECATED dVarCharValue "Use generic-lens or generic-optics with 'varCharValue' instead." #-}

instance Lude.FromJSON Datum where
  parseJSON =
    Lude.withObject
      "Datum"
      (\x -> Datum' Lude.<$> (x Lude..:? "VarCharValue"))
