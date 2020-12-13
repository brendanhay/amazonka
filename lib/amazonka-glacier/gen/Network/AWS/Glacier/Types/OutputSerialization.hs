{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputSerialization
  ( OutputSerialization (..),

    -- * Smart constructor
    mkOutputSerialization,

    -- * Lenses
    osCsv,
  )
where

import Network.AWS.Glacier.Types.CSVOutput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes how the select output is serialized.
--
-- /See:/ 'mkOutputSerialization' smart constructor.
newtype OutputSerialization = OutputSerialization'
  { -- | Describes the serialization of CSV-encoded query results.
    csv :: Lude.Maybe CSVOutput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- * 'csv' - Describes the serialization of CSV-encoded query results.
mkOutputSerialization ::
  OutputSerialization
mkOutputSerialization = OutputSerialization' {csv = Lude.Nothing}

-- | Describes the serialization of CSV-encoded query results.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCsv :: Lens.Lens' OutputSerialization (Lude.Maybe CSVOutput)
osCsv = Lens.lens (csv :: OutputSerialization -> Lude.Maybe CSVOutput) (\s a -> s {csv = a} :: OutputSerialization)
{-# DEPRECATED osCsv "Use generic-lens or generic-optics with 'csv' instead." #-}

instance Lude.FromJSON OutputSerialization where
  parseJSON =
    Lude.withObject
      "OutputSerialization"
      (\x -> OutputSerialization' Lude.<$> (x Lude..:? "csv"))

instance Lude.ToJSON OutputSerialization where
  toJSON OutputSerialization' {..} =
    Lude.object (Lude.catMaybes [("csv" Lude..=) Lude.<$> csv])
