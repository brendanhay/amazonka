-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TerminologyData
  ( TerminologyData (..),

    -- * Smart constructor
    mkTerminologyData,

    -- * Lenses
    tdFile,
    tdFormat,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.TerminologyDataFormat

-- | The data associated with the custom terminology.
--
-- /See:/ 'mkTerminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { file ::
      Lude.Sensitive Lude.Base64,
    format :: TerminologyDataFormat
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminologyData' with the minimum fields required to make a request.
--
-- * 'file' - The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'format' - The data format of the custom terminology. Either CSV or TMX.
mkTerminologyData ::
  -- | 'file'
  Lude.Sensitive Lude.Base64 ->
  -- | 'format'
  TerminologyDataFormat ->
  TerminologyData
mkTerminologyData pFile_ pFormat_ =
  TerminologyData' {file = pFile_, format = pFormat_}

-- | The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'file' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFile :: Lens.Lens' TerminologyData (Lude.Sensitive Lude.Base64)
tdFile = Lens.lens (file :: TerminologyData -> Lude.Sensitive Lude.Base64) (\s a -> s {file = a} :: TerminologyData)
{-# DEPRECATED tdFile "Use generic-lens or generic-optics with 'file' instead." #-}

-- | The data format of the custom terminology. Either CSV or TMX.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFormat :: Lens.Lens' TerminologyData TerminologyDataFormat
tdFormat = Lens.lens (format :: TerminologyData -> TerminologyDataFormat) (\s a -> s {format = a} :: TerminologyData)
{-# DEPRECATED tdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.ToJSON TerminologyData where
  toJSON TerminologyData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("File" Lude..= file),
            Lude.Just ("Format" Lude..= format)
          ]
      )
