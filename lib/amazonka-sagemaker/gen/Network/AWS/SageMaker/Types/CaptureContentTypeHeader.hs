-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureContentTypeHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureContentTypeHeader
  ( CaptureContentTypeHeader (..),

    -- * Smart constructor
    mkCaptureContentTypeHeader,

    -- * Lenses
    ccthCSVContentTypes,
    ccthJSONContentTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkCaptureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { csvContentTypes ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    jsonContentTypes ::
      Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptureContentTypeHeader' with the minimum fields required to make a request.
--
-- * 'csvContentTypes' -
-- * 'jsonContentTypes' -
mkCaptureContentTypeHeader ::
  CaptureContentTypeHeader
mkCaptureContentTypeHeader =
  CaptureContentTypeHeader'
    { csvContentTypes = Lude.Nothing,
      jsonContentTypes = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'csvContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccthCSVContentTypes :: Lens.Lens' CaptureContentTypeHeader (Lude.Maybe (Lude.NonEmpty Lude.Text))
ccthCSVContentTypes = Lens.lens (csvContentTypes :: CaptureContentTypeHeader -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {csvContentTypes = a} :: CaptureContentTypeHeader)
{-# DEPRECATED ccthCSVContentTypes "Use generic-lens or generic-optics with 'csvContentTypes' instead." #-}

-- |
--
-- /Note:/ Consider using 'jsonContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccthJSONContentTypes :: Lens.Lens' CaptureContentTypeHeader (Lude.Maybe (Lude.NonEmpty Lude.Text))
ccthJSONContentTypes = Lens.lens (jsonContentTypes :: CaptureContentTypeHeader -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {jsonContentTypes = a} :: CaptureContentTypeHeader)
{-# DEPRECATED ccthJSONContentTypes "Use generic-lens or generic-optics with 'jsonContentTypes' instead." #-}

instance Lude.FromJSON CaptureContentTypeHeader where
  parseJSON =
    Lude.withObject
      "CaptureContentTypeHeader"
      ( \x ->
          CaptureContentTypeHeader'
            Lude.<$> (x Lude..:? "CsvContentTypes")
            Lude.<*> (x Lude..:? "JsonContentTypes")
      )

instance Lude.ToJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CsvContentTypes" Lude..=) Lude.<$> csvContentTypes,
            ("JsonContentTypes" Lude..=) Lude.<$> jsonContentTypes
          ]
      )
