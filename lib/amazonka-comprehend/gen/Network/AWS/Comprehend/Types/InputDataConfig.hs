{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.InputDataConfig
  ( InputDataConfig (..)
  -- * Smart constructor
  , mkInputDataConfig
  -- * Lenses
  , idcS3Uri
  , idcInputFormat
  ) where

import qualified Network.AWS.Comprehend.Types.InputFormat as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The input properties for a topic detection job.
--
-- /See:/ 'mkInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { s3Uri :: Types.S3Uri
    -- ^ The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files. 
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
  , inputFormat :: Core.Maybe Types.InputFormat
    -- ^ Specifies how the text in an input file should be processed:
--
--
--     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.
--
--
--     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDataConfig' value with any optional fields omitted.
mkInputDataConfig
    :: Types.S3Uri -- ^ 's3Uri'
    -> InputDataConfig
mkInputDataConfig s3Uri
  = InputDataConfig'{s3Uri, inputFormat = Core.Nothing}

-- | The Amazon S3 URI for the input data. The URI must be in same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of data files. 
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcS3Uri :: Lens.Lens' InputDataConfig Types.S3Uri
idcS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE idcS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | Specifies how the text in an input file should be processed:
--
--
--     * @ONE_DOC_PER_FILE@ - Each file is considered a separate document. Use this option when you are processing large documents, such as newspaper articles or scientific papers.
--
--
--     * @ONE_DOC_PER_LINE@ - Each line in a file is considered a separate document. Use this option when you are processing many short documents, such as text messages.
--
--
--
-- /Note:/ Consider using 'inputFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcInputFormat :: Lens.Lens' InputDataConfig (Core.Maybe Types.InputFormat)
idcInputFormat = Lens.field @"inputFormat"
{-# INLINEABLE idcInputFormat #-}
{-# DEPRECATED inputFormat "Use generic-lens or generic-optics with 'inputFormat' instead"  #-}

instance Core.FromJSON InputDataConfig where
        toJSON InputDataConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  ("InputFormat" Core..=) Core.<$> inputFormat])

instance Core.FromJSON InputDataConfig where
        parseJSON
          = Core.withObject "InputDataConfig" Core.$
              \ x ->
                InputDataConfig' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..:? "InputFormat"
