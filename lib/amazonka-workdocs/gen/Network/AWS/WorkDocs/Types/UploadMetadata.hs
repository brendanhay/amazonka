{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UploadMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UploadMetadata
  ( UploadMetadata (..),

    -- * Smart constructor
    mkUploadMetadata,

    -- * Lenses
    umUploadURL,
    umSignedHeaders,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the upload.
--
-- /See:/ 'mkUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { -- | The URL of the upload.
    uploadURL :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The signed headers.
    signedHeaders :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadMetadata' with the minimum fields required to make a request.
--
-- * 'uploadURL' - The URL of the upload.
-- * 'signedHeaders' - The signed headers.
mkUploadMetadata ::
  UploadMetadata
mkUploadMetadata =
  UploadMetadata'
    { uploadURL = Lude.Nothing,
      signedHeaders = Lude.Nothing
    }

-- | The URL of the upload.
--
-- /Note:/ Consider using 'uploadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umUploadURL :: Lens.Lens' UploadMetadata (Lude.Maybe (Lude.Sensitive Lude.Text))
umUploadURL = Lens.lens (uploadURL :: UploadMetadata -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {uploadURL = a} :: UploadMetadata)
{-# DEPRECATED umUploadURL "Use generic-lens or generic-optics with 'uploadURL' instead." #-}

-- | The signed headers.
--
-- /Note:/ Consider using 'signedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umSignedHeaders :: Lens.Lens' UploadMetadata (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
umSignedHeaders = Lens.lens (signedHeaders :: UploadMetadata -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {signedHeaders = a} :: UploadMetadata)
{-# DEPRECATED umSignedHeaders "Use generic-lens or generic-optics with 'signedHeaders' instead." #-}

instance Lude.FromJSON UploadMetadata where
  parseJSON =
    Lude.withObject
      "UploadMetadata"
      ( \x ->
          UploadMetadata'
            Lude.<$> (x Lude..:? "UploadUrl")
            Lude.<*> (x Lude..:? "SignedHeaders" Lude..!= Lude.mempty)
      )
