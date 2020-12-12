{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InputSerialization
  ( InputSerialization (..),

    -- * Smart constructor
    mkInputSerialization,

    -- * Lenses
    isCsv,
  )
where

import Network.AWS.Glacier.Types.CSVInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes how the archive is serialized.
--
-- /See:/ 'mkInputSerialization' smart constructor.
newtype InputSerialization = InputSerialization'
  { csv ::
      Lude.Maybe CSVInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSerialization' with the minimum fields required to make a request.
--
-- * 'csv' - Describes the serialization of a CSV-encoded object.
mkInputSerialization ::
  InputSerialization
mkInputSerialization = InputSerialization' {csv = Lude.Nothing}

-- | Describes the serialization of a CSV-encoded object.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCsv :: Lens.Lens' InputSerialization (Lude.Maybe CSVInput)
isCsv = Lens.lens (csv :: InputSerialization -> Lude.Maybe CSVInput) (\s a -> s {csv = a} :: InputSerialization)
{-# DEPRECATED isCsv "Use generic-lens or generic-optics with 'csv' instead." #-}

instance Lude.FromJSON InputSerialization where
  parseJSON =
    Lude.withObject
      "InputSerialization"
      (\x -> InputSerialization' Lude.<$> (x Lude..:? "csv"))

instance Lude.ToJSON InputSerialization where
  toJSON InputSerialization' {..} =
    Lude.object (Lude.catMaybes [("csv" Lude..=) Lude.<$> csv])
