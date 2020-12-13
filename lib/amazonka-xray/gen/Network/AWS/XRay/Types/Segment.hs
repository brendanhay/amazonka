{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Segment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Segment
  ( Segment (..),

    -- * Smart constructor
    mkSegment,

    -- * Lenses
    sDocument,
    sId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with 'PutTraceSegments' , or an @inferred@ segment for a downstream service, generated from a subsegment sent by the service that called it.
--
-- For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
--
-- /See:/ 'mkSegment' smart constructor.
data Segment = Segment'
  { -- | The segment document.
    document :: Lude.Maybe Lude.Text,
    -- | The segment's ID.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Segment' with the minimum fields required to make a request.
--
-- * 'document' - The segment document.
-- * 'id' - The segment's ID.
mkSegment ::
  Segment
mkSegment = Segment' {document = Lude.Nothing, id = Lude.Nothing}

-- | The segment document.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDocument :: Lens.Lens' Segment (Lude.Maybe Lude.Text)
sDocument = Lens.lens (document :: Segment -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: Segment)
{-# DEPRECATED sDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The segment's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' Segment (Lude.Maybe Lude.Text)
sId = Lens.lens (id :: Segment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Segment)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Segment where
  parseJSON =
    Lude.withObject
      "Segment"
      ( \x ->
          Segment'
            Lude.<$> (x Lude..:? "Document") Lude.<*> (x Lude..:? "Id")
      )
