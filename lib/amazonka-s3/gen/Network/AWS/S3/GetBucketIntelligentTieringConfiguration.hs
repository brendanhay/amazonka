{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @GetBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.GetBucketIntelligentTieringConfiguration
  ( -- * Creating a request
    GetBucketIntelligentTieringConfiguration (..),
    mkGetBucketIntelligentTieringConfiguration,

    -- ** Request lenses
    gbitcBucket,
    gbitcId,

    -- * Destructuring the response
    GetBucketIntelligentTieringConfigurationResponse (..),
    mkGetBucketIntelligentTieringConfigurationResponse,

    -- ** Response lenses
    gbitcrsIntelligentTieringConfiguration,
    gbitcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketIntelligentTieringConfiguration' smart constructor.
data GetBucketIntelligentTieringConfiguration = GetBucketIntelligentTieringConfiguration'
  { bucket ::
      BucketName,
    id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
-- * 'id' - The ID used to identify the S3 Intelligent-Tiering configuration.
mkGetBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  GetBucketIntelligentTieringConfiguration
mkGetBucketIntelligentTieringConfiguration pBucket_ pId_ =
  GetBucketIntelligentTieringConfiguration'
    { bucket = pBucket_,
      id = pId_
    }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcBucket :: Lens.Lens' GetBucketIntelligentTieringConfiguration BucketName
gbitcBucket = Lens.lens (bucket :: GetBucketIntelligentTieringConfiguration -> BucketName) (\s a -> s {bucket = a} :: GetBucketIntelligentTieringConfiguration)
{-# DEPRECATED gbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcId :: Lens.Lens' GetBucketIntelligentTieringConfiguration Lude.Text
gbitcId = Lens.lens (id :: GetBucketIntelligentTieringConfiguration -> Lude.Text) (\s a -> s {id = a} :: GetBucketIntelligentTieringConfiguration)
{-# DEPRECATED gbitcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetBucketIntelligentTieringConfiguration where
  type
    Rs GetBucketIntelligentTieringConfiguration =
      GetBucketIntelligentTieringConfigurationResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketIntelligentTieringConfigurationResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketIntelligentTieringConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetBucketIntelligentTieringConfiguration where
  toPath GetBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketIntelligentTieringConfiguration where
  toQuery GetBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "intelligent-tiering"]

-- | /See:/ 'mkGetBucketIntelligentTieringConfigurationResponse' smart constructor.
data GetBucketIntelligentTieringConfigurationResponse = GetBucketIntelligentTieringConfigurationResponse'
  { intelligentTieringConfiguration ::
      Lude.Maybe
        IntelligentTieringConfiguration,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'intelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
-- * 'responseStatus' - The response status code.
mkGetBucketIntelligentTieringConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketIntelligentTieringConfigurationResponse
mkGetBucketIntelligentTieringConfigurationResponse pResponseStatus_ =
  GetBucketIntelligentTieringConfigurationResponse'
    { intelligentTieringConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Container for S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'intelligentTieringConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcrsIntelligentTieringConfiguration :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse (Lude.Maybe IntelligentTieringConfiguration)
gbitcrsIntelligentTieringConfiguration = Lens.lens (intelligentTieringConfiguration :: GetBucketIntelligentTieringConfigurationResponse -> Lude.Maybe IntelligentTieringConfiguration) (\s a -> s {intelligentTieringConfiguration = a} :: GetBucketIntelligentTieringConfigurationResponse)
{-# DEPRECATED gbitcrsIntelligentTieringConfiguration "Use generic-lens or generic-optics with 'intelligentTieringConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbitcrsResponseStatus :: Lens.Lens' GetBucketIntelligentTieringConfigurationResponse Lude.Int
gbitcrsResponseStatus = Lens.lens (responseStatus :: GetBucketIntelligentTieringConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketIntelligentTieringConfigurationResponse)
{-# DEPRECATED gbitcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
