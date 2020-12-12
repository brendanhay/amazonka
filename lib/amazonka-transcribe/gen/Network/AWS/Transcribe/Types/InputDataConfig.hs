{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.InputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.InputDataConfig
  ( InputDataConfig (..),

    -- * Smart constructor
    mkInputDataConfig,

    -- * Lenses
    idcTuningDataS3URI,
    idcS3URI,
    idcDataAccessRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The object that contains the Amazon S3 object location and access role required to train and tune your custom language model.
--
-- /See:/ 'mkInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { tuningDataS3URI ::
      Lude.Maybe Lude.Text,
    s3URI :: Lude.Text,
    dataAccessRoleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
-- * 's3URI' - The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
-- * 'tuningDataS3URI' - The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
mkInputDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  InputDataConfig
mkInputDataConfig pS3URI_ pDataAccessRoleARN_ =
  InputDataConfig'
    { tuningDataS3URI = Lude.Nothing,
      s3URI = pS3URI_,
      dataAccessRoleARN = pDataAccessRoleARN_
    }

-- | The Amazon S3 prefix you specify to access the plain text files that you use to tune your custom language model.
--
-- /Note:/ Consider using 'tuningDataS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcTuningDataS3URI :: Lens.Lens' InputDataConfig (Lude.Maybe Lude.Text)
idcTuningDataS3URI = Lens.lens (tuningDataS3URI :: InputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {tuningDataS3URI = a} :: InputDataConfig)
{-# DEPRECATED idcTuningDataS3URI "Use generic-lens or generic-optics with 'tuningDataS3URI' instead." #-}

-- | The Amazon S3 prefix you specify to access the plain text files that you use to train your custom language model.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcS3URI :: Lens.Lens' InputDataConfig Lude.Text
idcS3URI = Lens.lens (s3URI :: InputDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: InputDataConfig)
{-# DEPRECATED idcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

-- | The Amazon Resource Name (ARN) that uniquely identifies the permissions you've given Amazon Transcribe to access your Amazon S3 buckets containing your media files or text data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idcDataAccessRoleARN :: Lens.Lens' InputDataConfig Lude.Text
idcDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: InputDataConfig -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: InputDataConfig)
{-# DEPRECATED idcDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

instance Lude.FromJSON InputDataConfig where
  parseJSON =
    Lude.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Lude.<$> (x Lude..:? "TuningDataS3Uri")
            Lude.<*> (x Lude..: "S3Uri")
            Lude.<*> (x Lude..: "DataAccessRoleArn")
      )

instance Lude.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TuningDataS3Uri" Lude..=) Lude.<$> tuningDataS3URI,
            Lude.Just ("S3Uri" Lude..= s3URI),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN)
          ]
      )
