-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Attachment
  ( Attachment (..),

    -- * Smart constructor
    mkAttachment,

    -- * Lenses
    aData,
    aFileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An attachment to a case communication. The attachment consists of the file name and the content of the file.
--
-- /See:/ 'mkAttachment' smart constructor.
data Attachment = Attachment'
  { data' :: Lude.Maybe Lude.Base64,
    fileName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- * 'data'' - The content of the attachment file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'fileName' - The name of the attachment file.
mkAttachment ::
  Attachment
mkAttachment =
  Attachment' {data' = Lude.Nothing, fileName = Lude.Nothing}

-- | The content of the attachment file.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aData :: Lens.Lens' Attachment (Lude.Maybe Lude.Base64)
aData = Lens.lens (data' :: Attachment -> Lude.Maybe Lude.Base64) (\s a -> s {data' = a} :: Attachment)
{-# DEPRECATED aData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The name of the attachment file.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aFileName :: Lens.Lens' Attachment (Lude.Maybe Lude.Text)
aFileName = Lens.lens (fileName :: Attachment -> Lude.Maybe Lude.Text) (\s a -> s {fileName = a} :: Attachment)
{-# DEPRECATED aFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Lude.FromJSON Attachment where
  parseJSON =
    Lude.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Lude.<$> (x Lude..:? "data") Lude.<*> (x Lude..:? "fileName")
      )

instance Lude.ToJSON Attachment where
  toJSON Attachment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("data" Lude..=) Lude.<$> data',
            ("fileName" Lude..=) Lude.<$> fileName
          ]
      )
