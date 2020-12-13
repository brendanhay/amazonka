{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningSignature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningSignature
  ( CodeSigningSignature (..),

    -- * Smart constructor
    mkCodeSigningSignature,

    -- * Lenses
    cssInlineDocument,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the signature for a file.
--
-- /See:/ 'mkCodeSigningSignature' smart constructor.
newtype CodeSigningSignature = CodeSigningSignature'
  { -- | A base64 encoded binary representation of the code signing signature.
    inlineDocument :: Lude.Maybe Lude.Base64
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSigningSignature' with the minimum fields required to make a request.
--
-- * 'inlineDocument' - A base64 encoded binary representation of the code signing signature.
mkCodeSigningSignature ::
  CodeSigningSignature
mkCodeSigningSignature =
  CodeSigningSignature' {inlineDocument = Lude.Nothing}

-- | A base64 encoded binary representation of the code signing signature.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'inlineDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssInlineDocument :: Lens.Lens' CodeSigningSignature (Lude.Maybe Lude.Base64)
cssInlineDocument = Lens.lens (inlineDocument :: CodeSigningSignature -> Lude.Maybe Lude.Base64) (\s a -> s {inlineDocument = a} :: CodeSigningSignature)
{-# DEPRECATED cssInlineDocument "Use generic-lens or generic-optics with 'inlineDocument' instead." #-}

instance Lude.FromJSON CodeSigningSignature where
  parseJSON =
    Lude.withObject
      "CodeSigningSignature"
      ( \x ->
          CodeSigningSignature' Lude.<$> (x Lude..:? "inlineDocument")
      )

instance Lude.ToJSON CodeSigningSignature where
  toJSON CodeSigningSignature' {..} =
    Lude.object
      ( Lude.catMaybes
          [("inlineDocument" Lude..=) Lude.<$> inlineDocument]
      )
