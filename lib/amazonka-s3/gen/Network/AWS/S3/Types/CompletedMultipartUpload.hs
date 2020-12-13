{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CompletedMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CompletedMultipartUpload
  ( CompletedMultipartUpload (..),

    -- * Smart constructor
    mkCompletedMultipartUpload,

    -- * Lenses
    cmuParts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CompletedPart

-- | The container for the completed multipart upload details.
--
-- /See:/ 'mkCompletedMultipartUpload' smart constructor.
newtype CompletedMultipartUpload = CompletedMultipartUpload'
  { -- | Array of CompletedPart data types.
    parts :: Lude.Maybe (Lude.NonEmpty CompletedPart)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompletedMultipartUpload' with the minimum fields required to make a request.
--
-- * 'parts' - Array of CompletedPart data types.
mkCompletedMultipartUpload ::
  CompletedMultipartUpload
mkCompletedMultipartUpload =
  CompletedMultipartUpload' {parts = Lude.Nothing}

-- | Array of CompletedPart data types.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuParts :: Lens.Lens' CompletedMultipartUpload (Lude.Maybe (Lude.NonEmpty CompletedPart))
cmuParts = Lens.lens (parts :: CompletedMultipartUpload -> Lude.Maybe (Lude.NonEmpty CompletedPart)) (\s a -> s {parts = a} :: CompletedMultipartUpload)
{-# DEPRECATED cmuParts "Use generic-lens or generic-optics with 'parts' instead." #-}

instance Lude.ToXML CompletedMultipartUpload where
  toXML CompletedMultipartUpload' {..} =
    Lude.mconcat [Lude.toXML (Lude.toXMLList "Part" Lude.<$> parts)]
