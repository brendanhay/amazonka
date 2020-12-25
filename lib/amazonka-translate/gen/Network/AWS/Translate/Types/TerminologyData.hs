{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.TerminologyDataFormat as Types

-- | The data associated with the custom terminology.
--
-- /See:/ 'mkTerminologyData' smart constructor.
data TerminologyData = TerminologyData'
  { -- | The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.
    file :: Core.Sensitive Core.Base64,
    -- | The data format of the custom terminology. Either CSV or TMX.
    format :: Types.TerminologyDataFormat
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminologyData' value with any optional fields omitted.
mkTerminologyData ::
  -- | 'file'
  Core.Sensitive Core.Base64 ->
  -- | 'format'
  Types.TerminologyDataFormat ->
  TerminologyData
mkTerminologyData file format = TerminologyData' {file, format}

-- | The file containing the custom terminology data. Your version of the AWS SDK performs a Base64-encoding on this field before sending a request to the AWS service. Users of the SDK should not perform Base64-encoding themselves.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'file' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFile :: Lens.Lens' TerminologyData (Core.Sensitive Core.Base64)
tdFile = Lens.field @"file"
{-# DEPRECATED tdFile "Use generic-lens or generic-optics with 'file' instead." #-}

-- | The data format of the custom terminology. Either CSV or TMX.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdFormat :: Lens.Lens' TerminologyData Types.TerminologyDataFormat
tdFormat = Lens.field @"format"
{-# DEPRECATED tdFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Core.FromJSON TerminologyData where
  toJSON TerminologyData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("File" Core..= file),
            Core.Just ("Format" Core..= format)
          ]
      )
