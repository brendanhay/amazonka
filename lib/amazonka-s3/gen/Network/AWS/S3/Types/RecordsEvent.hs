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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The container for the records event.
--
-- /See:/ 'mkRecordsEvent' smart constructor.
newtype RecordsEvent = RecordsEvent'
  { -- | The byte array of partial, one or more result records.
    payload :: Core.Maybe Core.Base64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RecordsEvent' value with any optional fields omitted.
mkRecordsEvent ::
  RecordsEvent
mkRecordsEvent = RecordsEvent' {payload = Core.Nothing}

-- | The byte array of partial, one or more result records.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rePayload :: Lens.Lens' RecordsEvent (Core.Maybe Core.Base64)
rePayload = Lens.field @"payload"
{-# DEPRECATED rePayload "Use generic-lens or generic-optics with 'payload' instead." #-}

instance Core.FromXML RecordsEvent where
  parseXML x = RecordsEvent' Core.<$> (x Core..@? "Payload")
