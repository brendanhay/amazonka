{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RecordsEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RecordsEvent
  ( RecordsEvent (..),

    -- * Smart constructor
    mkRecordsEvent,

    -- * Lenses
    rePayload,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The container for the records event.
--
-- /See:/ 'mkRecordsEvent' smart constructor.
newtype RecordsEvent = RecordsEvent'
  { -- | The byte array of partial, one or more result records.
    payload :: Lude.Maybe Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordsEvent' with the minimum fields required to make a request.
--
-- * 'payload' - The byte array of partial, one or more result records.
mkRecordsEvent ::
  RecordsEvent
mkRecordsEvent = RecordsEvent' {payload = Lude.Nothing}

-- | The byte array of partial, one or more result records.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rePayload :: Lens.Lens' RecordsEvent (Lude.Maybe Lude.Base64)
rePayload = Lens.lens (payload :: RecordsEvent -> Lude.Maybe Lude.Base64) (\s a -> s {payload = a} :: RecordsEvent)
{-# DEPRECATED rePayload "Use generic-lens or generic-optics with 'payload' instead." #-}

instance Lude.FromXML RecordsEvent where
  parseXML x = RecordsEvent' Lude.<$> (x Lude..@? "Payload")
