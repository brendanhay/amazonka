{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OutputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputSerialization
  ( OutputSerialization (..),

    -- * Smart constructor
    mkOutputSerialization,

    -- * Lenses
    osJSON,
    osCSV,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CSVOutput
import Network.AWS.S3.Types.JSONOutput

-- | Describes how results of the Select job are serialized.
--
-- /See:/ 'mkOutputSerialization' smart constructor.
data OutputSerialization = OutputSerialization'
  { -- | Specifies JSON as request's output serialization format.
    json :: Lude.Maybe JSONOutput,
    -- | Describes the serialization of CSV-encoded Select results.
    csv :: Lude.Maybe CSVOutput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputSerialization' with the minimum fields required to make a request.
--
-- * 'json' - Specifies JSON as request's output serialization format.
-- * 'csv' - Describes the serialization of CSV-encoded Select results.
mkOutputSerialization ::
  OutputSerialization
mkOutputSerialization =
  OutputSerialization' {json = Lude.Nothing, csv = Lude.Nothing}

-- | Specifies JSON as request's output serialization format.
--
-- /Note:/ Consider using 'json' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osJSON :: Lens.Lens' OutputSerialization (Lude.Maybe JSONOutput)
osJSON = Lens.lens (json :: OutputSerialization -> Lude.Maybe JSONOutput) (\s a -> s {json = a} :: OutputSerialization)
{-# DEPRECATED osJSON "Use generic-lens or generic-optics with 'json' instead." #-}

-- | Describes the serialization of CSV-encoded Select results.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCSV :: Lens.Lens' OutputSerialization (Lude.Maybe CSVOutput)
osCSV = Lens.lens (csv :: OutputSerialization -> Lude.Maybe CSVOutput) (\s a -> s {csv = a} :: OutputSerialization)
{-# DEPRECATED osCSV "Use generic-lens or generic-optics with 'csv' instead." #-}

instance Lude.ToXML OutputSerialization where
  toXML OutputSerialization' {..} =
    Lude.mconcat ["JSON" Lude.@= json, "CSV" Lude.@= csv]
