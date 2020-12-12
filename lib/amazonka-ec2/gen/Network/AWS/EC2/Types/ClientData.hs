{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientData
  ( ClientData (..),

    -- * Smart constructor
    mkClientData,

    -- * Lenses
    cdUploadStart,
    cdUploadSize,
    cdUploadEnd,
    cdComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the client-specific data.
--
-- /See:/ 'mkClientData' smart constructor.
data ClientData = ClientData'
  { uploadStart ::
      Lude.Maybe Lude.DateTime,
    uploadSize :: Lude.Maybe Lude.Double,
    uploadEnd :: Lude.Maybe Lude.DateTime,
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientData' with the minimum fields required to make a request.
--
-- * 'comment' - A user-defined comment about the disk upload.
-- * 'uploadEnd' - The time that the disk upload ends.
-- * 'uploadSize' - The size of the uploaded disk image, in GiB.
-- * 'uploadStart' - The time that the disk upload starts.
mkClientData ::
  ClientData
mkClientData =
  ClientData'
    { uploadStart = Lude.Nothing,
      uploadSize = Lude.Nothing,
      uploadEnd = Lude.Nothing,
      comment = Lude.Nothing
    }

-- | The time that the disk upload starts.
--
-- /Note:/ Consider using 'uploadStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadStart :: Lens.Lens' ClientData (Lude.Maybe Lude.DateTime)
cdUploadStart = Lens.lens (uploadStart :: ClientData -> Lude.Maybe Lude.DateTime) (\s a -> s {uploadStart = a} :: ClientData)
{-# DEPRECATED cdUploadStart "Use generic-lens or generic-optics with 'uploadStart' instead." #-}

-- | The size of the uploaded disk image, in GiB.
--
-- /Note:/ Consider using 'uploadSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadSize :: Lens.Lens' ClientData (Lude.Maybe Lude.Double)
cdUploadSize = Lens.lens (uploadSize :: ClientData -> Lude.Maybe Lude.Double) (\s a -> s {uploadSize = a} :: ClientData)
{-# DEPRECATED cdUploadSize "Use generic-lens or generic-optics with 'uploadSize' instead." #-}

-- | The time that the disk upload ends.
--
-- /Note:/ Consider using 'uploadEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdUploadEnd :: Lens.Lens' ClientData (Lude.Maybe Lude.DateTime)
cdUploadEnd = Lens.lens (uploadEnd :: ClientData -> Lude.Maybe Lude.DateTime) (\s a -> s {uploadEnd = a} :: ClientData)
{-# DEPRECATED cdUploadEnd "Use generic-lens or generic-optics with 'uploadEnd' instead." #-}

-- | A user-defined comment about the disk upload.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdComment :: Lens.Lens' ClientData (Lude.Maybe Lude.Text)
cdComment = Lens.lens (comment :: ClientData -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: ClientData)
{-# DEPRECATED cdComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.ToQuery ClientData where
  toQuery ClientData' {..} =
    Lude.mconcat
      [ "UploadStart" Lude.=: uploadStart,
        "UploadSize" Lude.=: uploadSize,
        "UploadEnd" Lude.=: uploadEnd,
        "Comment" Lude.=: comment
      ]
