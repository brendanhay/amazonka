{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningSignature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CodeSigningSignature
  ( CodeSigningSignature (..)
  -- * Smart constructor
  , mkCodeSigningSignature
  -- * Lenses
  , cssInlineDocument
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the signature for a file.
--
-- /See:/ 'mkCodeSigningSignature' smart constructor.
newtype CodeSigningSignature = CodeSigningSignature'
  { inlineDocument :: Core.Maybe Core.Base64
    -- ^ A base64 encoded binary representation of the code signing signature.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CodeSigningSignature' value with any optional fields omitted.
mkCodeSigningSignature
    :: CodeSigningSignature
mkCodeSigningSignature
  = CodeSigningSignature'{inlineDocument = Core.Nothing}

-- | A base64 encoded binary representation of the code signing signature.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'inlineDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssInlineDocument :: Lens.Lens' CodeSigningSignature (Core.Maybe Core.Base64)
cssInlineDocument = Lens.field @"inlineDocument"
{-# INLINEABLE cssInlineDocument #-}
{-# DEPRECATED inlineDocument "Use generic-lens or generic-optics with 'inlineDocument' instead"  #-}

instance Core.FromJSON CodeSigningSignature where
        toJSON CodeSigningSignature{..}
          = Core.object
              (Core.catMaybes
                 [("inlineDocument" Core..=) Core.<$> inlineDocument])

instance Core.FromJSON CodeSigningSignature where
        parseJSON
          = Core.withObject "CodeSigningSignature" Core.$
              \ x -> CodeSigningSignature' Core.<$> (x Core..:? "inlineDocument")
