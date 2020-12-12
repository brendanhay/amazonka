{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointInput
  ( EndpointInput (..),

    -- * Smart constructor
    mkEndpointInput,

    -- * Lenses
    eiS3DataDistributionType,
    eiS3InputMode,
    eiEndpointName,
    eiLocalPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
import Network.AWS.SageMaker.Types.ProcessingS3InputMode

-- | Input object for the endpoint
--
-- /See:/ 'mkEndpointInput' smart constructor.
data EndpointInput = EndpointInput'
  { s3DataDistributionType ::
      Lude.Maybe ProcessingS3DataDistributionType,
    s3InputMode :: Lude.Maybe ProcessingS3InputMode,
    endpointName :: Lude.Text,
    localPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointInput' with the minimum fields required to make a request.
--
-- * 'endpointName' - An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
-- * 'localPath' - Path to the filesystem where the endpoint data is available to the container.
-- * 's3DataDistributionType' - Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@
-- * 's3InputMode' - Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
mkEndpointInput ::
  -- | 'endpointName'
  Lude.Text ->
  -- | 'localPath'
  Lude.Text ->
  EndpointInput
mkEndpointInput pEndpointName_ pLocalPath_ =
  EndpointInput'
    { s3DataDistributionType = Lude.Nothing,
      s3InputMode = Lude.Nothing,
      endpointName = pEndpointName_,
      localPath = pLocalPath_
    }

-- | Whether input data distributed in Amazon S3 is fully replicated or sharded by an S3 key. Defauts to @FullyReplicated@
--
-- /Note:/ Consider using 's3DataDistributionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3DataDistributionType :: Lens.Lens' EndpointInput (Lude.Maybe ProcessingS3DataDistributionType)
eiS3DataDistributionType = Lens.lens (s3DataDistributionType :: EndpointInput -> Lude.Maybe ProcessingS3DataDistributionType) (\s a -> s {s3DataDistributionType = a} :: EndpointInput)
{-# DEPRECATED eiS3DataDistributionType "Use generic-lens or generic-optics with 's3DataDistributionType' instead." #-}

-- | Whether the @Pipe@ or @File@ is used as the input mode for transfering data for the monitoring job. @Pipe@ mode is recommended for large datasets. @File@ mode is useful for small files that fit in memory. Defaults to @File@ .
--
-- /Note:/ Consider using 's3InputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiS3InputMode :: Lens.Lens' EndpointInput (Lude.Maybe ProcessingS3InputMode)
eiS3InputMode = Lens.lens (s3InputMode :: EndpointInput -> Lude.Maybe ProcessingS3InputMode) (\s a -> s {s3InputMode = a} :: EndpointInput)
{-# DEPRECATED eiS3InputMode "Use generic-lens or generic-optics with 's3InputMode' instead." #-}

-- | An endpoint in customer's account which has enabled @DataCaptureConfig@ enabled.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEndpointName :: Lens.Lens' EndpointInput Lude.Text
eiEndpointName = Lens.lens (endpointName :: EndpointInput -> Lude.Text) (\s a -> s {endpointName = a} :: EndpointInput)
{-# DEPRECATED eiEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | Path to the filesystem where the endpoint data is available to the container.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiLocalPath :: Lens.Lens' EndpointInput Lude.Text
eiLocalPath = Lens.lens (localPath :: EndpointInput -> Lude.Text) (\s a -> s {localPath = a} :: EndpointInput)
{-# DEPRECATED eiLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

instance Lude.FromJSON EndpointInput where
  parseJSON =
    Lude.withObject
      "EndpointInput"
      ( \x ->
          EndpointInput'
            Lude.<$> (x Lude..:? "S3DataDistributionType")
            Lude.<*> (x Lude..:? "S3InputMode")
            Lude.<*> (x Lude..: "EndpointName")
            Lude.<*> (x Lude..: "LocalPath")
      )

instance Lude.ToJSON EndpointInput where
  toJSON EndpointInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3DataDistributionType" Lude..=)
              Lude.<$> s3DataDistributionType,
            ("S3InputMode" Lude..=) Lude.<$> s3InputMode,
            Lude.Just ("EndpointName" Lude..= endpointName),
            Lude.Just ("LocalPath" Lude..= localPath)
          ]
      )
