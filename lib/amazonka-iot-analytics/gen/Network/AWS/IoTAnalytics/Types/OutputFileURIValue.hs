-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.OutputFileURIValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.OutputFileURIValue
  ( OutputFileURIValue (..),

    -- * Smart constructor
    mkOutputFileURIValue,

    -- * Lenses
    ofuvFileName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value of the variable as a structure that specifies an output file URI.
--
-- /See:/ 'mkOutputFileURIValue' smart constructor.
newtype OutputFileURIValue = OutputFileURIValue'
  { fileName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputFileURIValue' with the minimum fields required to make a request.
--
-- * 'fileName' - The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
mkOutputFileURIValue ::
  -- | 'fileName'
  Lude.Text ->
  OutputFileURIValue
mkOutputFileURIValue pFileName_ =
  OutputFileURIValue' {fileName = pFileName_}

-- | The URI of the location where dataset contents are stored, usually the URI of a file in an S3 bucket.
--
-- /Note:/ Consider using 'fileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofuvFileName :: Lens.Lens' OutputFileURIValue Lude.Text
ofuvFileName = Lens.lens (fileName :: OutputFileURIValue -> Lude.Text) (\s a -> s {fileName = a} :: OutputFileURIValue)
{-# DEPRECATED ofuvFileName "Use generic-lens or generic-optics with 'fileName' instead." #-}

instance Lude.FromJSON OutputFileURIValue where
  parseJSON =
    Lude.withObject
      "OutputFileURIValue"
      (\x -> OutputFileURIValue' Lude.<$> (x Lude..: "fileName"))

instance Lude.ToJSON OutputFileURIValue where
  toJSON OutputFileURIValue' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("fileName" Lude..= fileName)])
