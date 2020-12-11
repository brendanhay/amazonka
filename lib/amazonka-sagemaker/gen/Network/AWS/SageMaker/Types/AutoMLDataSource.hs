-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLDataSource
  ( AutoMLDataSource (..),

    -- * Smart constructor
    mkAutoMLDataSource,

    -- * Lenses
    amldsS3DataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLS3DataSource

-- | The data source for the Autopilot job.
--
-- /See:/ 'mkAutoMLDataSource' smart constructor.
newtype AutoMLDataSource = AutoMLDataSource'
  { s3DataSource ::
      AutoMLS3DataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLDataSource' with the minimum fields required to make a request.
--
-- * 's3DataSource' - The Amazon S3 location of the input data.
mkAutoMLDataSource ::
  -- | 's3DataSource'
  AutoMLS3DataSource ->
  AutoMLDataSource
mkAutoMLDataSource pS3DataSource_ =
  AutoMLDataSource' {s3DataSource = pS3DataSource_}

-- | The Amazon S3 location of the input data.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amldsS3DataSource :: Lens.Lens' AutoMLDataSource AutoMLS3DataSource
amldsS3DataSource = Lens.lens (s3DataSource :: AutoMLDataSource -> AutoMLS3DataSource) (\s a -> s {s3DataSource = a} :: AutoMLDataSource)
{-# DEPRECATED amldsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

instance Lude.FromJSON AutoMLDataSource where
  parseJSON =
    Lude.withObject
      "AutoMLDataSource"
      (\x -> AutoMLDataSource' Lude.<$> (x Lude..: "S3DataSource"))

instance Lude.ToJSON AutoMLDataSource where
  toJSON AutoMLDataSource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("S3DataSource" Lude..= s3DataSource)])
