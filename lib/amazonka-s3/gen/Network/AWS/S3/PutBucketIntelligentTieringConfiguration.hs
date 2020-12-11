{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts a S3 Intelligent-Tiering configuration to the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @PutBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketIntelligentTieringConfiguration.html DeleteBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.PutBucketIntelligentTieringConfiguration
  ( -- * Creating a request
    PutBucketIntelligentTieringConfiguration (..),
    mkPutBucketIntelligentTieringConfiguration,

    -- ** Request lenses
    pbitcBucket,
    pbitcId,
    pbitcIntelligentTieringConfiguration,

    -- * Destructuring the response
    PutBucketIntelligentTieringConfigurationResponse (..),
    mkPutBucketIntelligentTieringConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketIntelligentTieringConfiguration' smart constructor.
data PutBucketIntelligentTieringConfiguration = PutBucketIntelligentTieringConfiguration'
  { bucket ::
      BucketName,
    id ::
      Lude.Text,
    intelligentTieringConfiguration ::
      IntelligentTieringConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
-- * 'id' - The ID used to identify the S3 Intelligent-Tiering configuration.
-- * 'intelligentTieringConfiguration' - Container for S3 Intelligent-Tiering configuration.
mkPutBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  -- | 'intelligentTieringConfiguration'
  IntelligentTieringConfiguration ->
  PutBucketIntelligentTieringConfiguration
mkPutBucketIntelligentTieringConfiguration
  pBucket_
  pId_
  pIntelligentTieringConfiguration_ =
    PutBucketIntelligentTieringConfiguration'
      { bucket = pBucket_,
        id = pId_,
        intelligentTieringConfiguration =
          pIntelligentTieringConfiguration_
      }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcBucket :: Lens.Lens' PutBucketIntelligentTieringConfiguration BucketName
pbitcBucket = Lens.lens (bucket :: PutBucketIntelligentTieringConfiguration -> BucketName) (\s a -> s {bucket = a} :: PutBucketIntelligentTieringConfiguration)
{-# DEPRECATED pbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcId :: Lens.Lens' PutBucketIntelligentTieringConfiguration Lude.Text
pbitcId = Lens.lens (id :: PutBucketIntelligentTieringConfiguration -> Lude.Text) (\s a -> s {id = a} :: PutBucketIntelligentTieringConfiguration)
{-# DEPRECATED pbitcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Container for S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'intelligentTieringConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbitcIntelligentTieringConfiguration :: Lens.Lens' PutBucketIntelligentTieringConfiguration IntelligentTieringConfiguration
pbitcIntelligentTieringConfiguration = Lens.lens (intelligentTieringConfiguration :: PutBucketIntelligentTieringConfiguration -> IntelligentTieringConfiguration) (\s a -> s {intelligentTieringConfiguration = a} :: PutBucketIntelligentTieringConfiguration)
{-# DEPRECATED pbitcIntelligentTieringConfiguration "Use generic-lens or generic-optics with 'intelligentTieringConfiguration' instead." #-}

instance Lude.AWSRequest PutBucketIntelligentTieringConfiguration where
  type
    Rs PutBucketIntelligentTieringConfiguration =
      PutBucketIntelligentTieringConfigurationResponse
  request = Req.putXML s3Service
  response =
    Res.receiveNull PutBucketIntelligentTieringConfigurationResponse'

instance Lude.ToElement PutBucketIntelligentTieringConfiguration where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}IntelligentTieringConfiguration"
      Lude.. intelligentTieringConfiguration

instance Lude.ToHeaders PutBucketIntelligentTieringConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutBucketIntelligentTieringConfiguration where
  toPath PutBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketIntelligentTieringConfiguration where
  toQuery PutBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "intelligent-tiering"]

-- | /See:/ 'mkPutBucketIntelligentTieringConfigurationResponse' smart constructor.
data PutBucketIntelligentTieringConfigurationResponse = PutBucketIntelligentTieringConfigurationResponse'
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

-- | Creates a value of 'PutBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
mkPutBucketIntelligentTieringConfigurationResponse ::
  PutBucketIntelligentTieringConfigurationResponse
mkPutBucketIntelligentTieringConfigurationResponse =
  PutBucketIntelligentTieringConfigurationResponse'
