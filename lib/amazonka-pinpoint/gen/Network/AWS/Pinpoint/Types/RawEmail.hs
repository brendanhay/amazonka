{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RawEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RawEmail
  ( RawEmail (..)
  -- * Smart constructor
  , mkRawEmail
  -- * Lenses
  , reData
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the contents of an email message, represented as a raw MIME message.
--
-- /See:/ 'mkRawEmail' smart constructor.
newtype RawEmail = RawEmail'
  { data' :: Core.Maybe Core.Base64
    -- ^ The email message, represented as a raw MIME message. The entire message must be base64 encoded.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RawEmail' value with any optional fields omitted.
mkRawEmail
    :: RawEmail
mkRawEmail = RawEmail'{data' = Core.Nothing}

-- | The email message, represented as a raw MIME message. The entire message must be base64 encoded.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reData :: Lens.Lens' RawEmail (Core.Maybe Core.Base64)
reData = Lens.field @"data'"
{-# INLINEABLE reData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

instance Core.FromJSON RawEmail where
        toJSON RawEmail{..}
          = Core.object (Core.catMaybes [("Data" Core..=) Core.<$> data'])
