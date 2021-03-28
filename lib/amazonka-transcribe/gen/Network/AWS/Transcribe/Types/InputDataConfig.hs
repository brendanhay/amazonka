{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.InputDataConfig
  ( InputDataConfig (..)
  -- * Smart constructor
  , mkInputDataConfig
  -- * Lenses
  , idcS3Uri
  , idcDataAccessRoleArn
  , idcTuningDataS3Uri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.DataAccessRoleArn as Types
import qualified Network.AWS.Transcribe.Types.Uri as Types

-- | The object that contains the Amazon S3 object location and access role required to train and tune your custom language model.
--
-- /See:/ 'mkInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { s3Uri :: Types.Uri
    -- ^ The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
  , dataAccessRoleArn :: Types.DataAccessRoleArn
    -- ^ The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
  , tuningDataS3Uri :: Core.Maybe Types.Uri
    -- ^ The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDataConfig' value with any optional fields omitted.
mkInputDataConfig
    :: Types.Uri -- ^ 's3Uri'
    -> Types.DataAccessRoleArn -- ^ 'dataAccessRoleArn'
    -> InputDataConfig
mkInputDataConfig s3Uri dataAccessRoleArn
  = InputDataConfig'{s3Uri, dataAccessRoleArn,
                     tuningDataS3Uri = Core.Nothing}

-- | The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcS3Uri :: Lens.Lens' InputDataConfig Types.Uri
idcS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE idcS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDataAccessRoleArn :: Lens.Lens' InputDataConfig Types.DataAccessRoleArn
idcDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# INLINEABLE idcDataAccessRoleArn #-}
{-# DEPRECATED dataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead"  #-}

-- | The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
--
-- /Note:/ Consider using 'tuningDataS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcTuningDataS3Uri :: Lens.Lens' InputDataConfig (Core.Maybe Types.Uri)
idcTuningDataS3Uri = Lens.field @"tuningDataS3Uri"
{-# INLINEABLE idcTuningDataS3Uri #-}
{-# DEPRECATED tuningDataS3Uri "Use generic-lens or generic-optics with 'tuningDataS3Uri' instead"  #-}

instance Core.FromJSON InputDataConfig where
        toJSON InputDataConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
                  ("TuningDataS3Uri" Core..=) Core.<$> tuningDataS3Uri])

instance Core.FromJSON InputDataConfig where
        parseJSON
          = Core.withObject "InputDataConfig" Core.$
              \ x ->
                InputDataConfig' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..: "DataAccessRoleArn" Core.<*>
                    x Core..:? "TuningDataS3Uri"
