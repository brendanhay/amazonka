{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the S3 Intelligent-Tiering configuration from the specified bucket.
--
-- The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead. S3 Intelligent-Tiering delivers automatic cost savings by moving data between access tiers, when access patterns change.
-- The S3 Intelligent-Tiering storage class is suitable for objects larger than 128 KB that you plan to store for at least 30 days. If the size of an object is less than 128 KB, it is not eligible for auto-tiering. Smaller objects can be stored, but they are always charged at the frequent access tier rates in the S3 Intelligent-Tiering storage class.
-- If you delete an object before the end of the 30-day minimum storage duration period, you are charged for 30 days. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> .
-- Operations related to @DeleteBucketIntelligentTieringConfiguration@ include:
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketIntelligentTieringConfiguration.html GetBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketIntelligentTieringConfiguration.html PutBucketIntelligentTieringConfiguration>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketIntelligentTieringConfigurations.html ListBucketIntelligentTieringConfigurations>
module Network.AWS.S3.DeleteBucketIntelligentTieringConfiguration
  ( -- * Creating a request
    DeleteBucketIntelligentTieringConfiguration (..),
    mkDeleteBucketIntelligentTieringConfiguration,

    -- ** Request lenses
    dbitcBucket,
    dbitcId,

    -- * Destructuring the response
    DeleteBucketIntelligentTieringConfigurationResponse (..),
    mkDeleteBucketIntelligentTieringConfigurationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketIntelligentTieringConfiguration' smart constructor.
data DeleteBucketIntelligentTieringConfiguration = DeleteBucketIntelligentTieringConfiguration'
  { -- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketIntelligentTieringConfiguration' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
-- * 'id' - The ID used to identify the S3 Intelligent-Tiering configuration.
mkDeleteBucketIntelligentTieringConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Lude.Text ->
  DeleteBucketIntelligentTieringConfiguration
mkDeleteBucketIntelligentTieringConfiguration pBucket_ pId_ =
  DeleteBucketIntelligentTieringConfiguration'
    { bucket = pBucket_,
      id = pId_
    }

-- | The name of the Amazon S3 bucket whose configuration you want to modify or retrieve.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbitcBucket :: Lens.Lens' DeleteBucketIntelligentTieringConfiguration BucketName
dbitcBucket = Lens.lens (bucket :: DeleteBucketIntelligentTieringConfiguration -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketIntelligentTieringConfiguration)
{-# DEPRECATED dbitcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbitcId :: Lens.Lens' DeleteBucketIntelligentTieringConfiguration Lude.Text
dbitcId = Lens.lens (id :: DeleteBucketIntelligentTieringConfiguration -> Lude.Text) (\s a -> s {id = a} :: DeleteBucketIntelligentTieringConfiguration)
{-# DEPRECATED dbitcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance
  Lude.AWSRequest
    DeleteBucketIntelligentTieringConfiguration
  where
  type
    Rs DeleteBucketIntelligentTieringConfiguration =
      DeleteBucketIntelligentTieringConfigurationResponse
  request = Req.delete s3Service
  response =
    Res.receiveNull
      DeleteBucketIntelligentTieringConfigurationResponse'

instance Lude.ToHeaders DeleteBucketIntelligentTieringConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteBucketIntelligentTieringConfiguration where
  toPath DeleteBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketIntelligentTieringConfiguration where
  toQuery DeleteBucketIntelligentTieringConfiguration' {..} =
    Lude.mconcat ["id" Lude.=: id, "intelligent-tiering"]

-- | /See:/ 'mkDeleteBucketIntelligentTieringConfigurationResponse' smart constructor.
data DeleteBucketIntelligentTieringConfigurationResponse = DeleteBucketIntelligentTieringConfigurationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketIntelligentTieringConfigurationResponse' with the minimum fields required to make a request.
mkDeleteBucketIntelligentTieringConfigurationResponse ::
  DeleteBucketIntelligentTieringConfigurationResponse
mkDeleteBucketIntelligentTieringConfigurationResponse =
  DeleteBucketIntelligentTieringConfigurationResponse'
