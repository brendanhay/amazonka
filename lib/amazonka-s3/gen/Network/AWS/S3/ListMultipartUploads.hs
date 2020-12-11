{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists in-progress multipart uploads. An in-progress multipart upload is a multipart upload that has been initiated using the Initiate Multipart Upload request, but has not yet been completed or aborted.
--
-- This operation returns at most 1,000 multipart uploads in the response. 1,000 multipart uploads is the maximum number of uploads a response can include, which is also the default value. You can further limit the number of uploads in a response by specifying the @max-uploads@ parameter in the response. If additional multipart uploads satisfy the list criteria, the response will contain an @IsTruncated@ element with the value true. To list the additional multipart uploads, use the @key-marker@ and @upload-id-marker@ request parameters.
-- In the response, the uploads are sorted by key. If your application has initiated more than one multipart upload using the same object key, then uploads in the response are first sorted by key. Additionally, uploads are sorted in ascending order within each key by the upload initiation time.
-- For more information on multipart uploads, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/uploadobjusingmpu.html Uploading Objects Using Multipart Upload> .
-- For information on permissions required to use the multipart upload API, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuAndPermissions.html Multipart Upload API and Permissions> .
-- The following operations are related to @ListMultipartUploads@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListMultipartUploads
  ( -- * Creating a request
    ListMultipartUploads (..),
    mkListMultipartUploads,

    -- ** Request lenses
    lmuKeyMarker,
    lmuPrefix,
    lmuEncodingType,
    lmuUploadIdMarker,
    lmuMaxUploads,
    lmuDelimiter,
    lmuExpectedBucketOwner,
    lmuBucket,

    -- * Destructuring the response
    ListMultipartUploadsResponse (..),
    mkListMultipartUploadsResponse,

    -- ** Response lenses
    lmursKeyMarker,
    lmursPrefix,
    lmursCommonPrefixes,
    lmursEncodingType,
    lmursBucket,
    lmursUploadIdMarker,
    lmursMaxUploads,
    lmursNextKeyMarker,
    lmursUploads,
    lmursIsTruncated,
    lmursNextUploadIdMarker,
    lmursDelimiter,
    lmursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListMultipartUploads' smart constructor.
data ListMultipartUploads = ListMultipartUploads'
  { keyMarker ::
      Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    encodingType :: Lude.Maybe EncodingType,
    uploadIdMarker :: Lude.Maybe Lude.Text,
    maxUploads :: Lude.Maybe Lude.Int,
    delimiter :: Lude.Maybe Delimiter,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultipartUploads' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'delimiter' - Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified, and the first occurrence of the delimiter after the prefix are grouped under a single result element, @CommonPrefixes@ . If you don't specify the prefix parameter, then the substring starts at the beginning of the key. The keys that are grouped under @CommonPrefixes@ result element are not returned elsewhere in the response.
-- * 'encodingType' - Undocumented field.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'keyMarker' - Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically greater than the specified @key-marker@ will be included in the list.
-- If @upload-id-marker@ is specified, any multipart uploads for a key equal to the @key-marker@ might also be included, provided those multipart uploads have upload IDs lexicographically greater than the specified @upload-id-marker@ .
-- * 'maxUploads' - Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
-- * 'prefix' - Lists in-progress uploads only for those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different grouping of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.)
-- * 'uploadIdMarker' - Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored. Otherwise, any multipart uploads for a key equal to the key-marker might be included in the list only if they have an upload ID lexicographically greater than the specified @upload-id-marker@ .
mkListMultipartUploads ::
  -- | 'bucket'
  BucketName ->
  ListMultipartUploads
mkListMultipartUploads pBucket_ =
  ListMultipartUploads'
    { keyMarker = Lude.Nothing,
      prefix = Lude.Nothing,
      encodingType = Lude.Nothing,
      uploadIdMarker = Lude.Nothing,
      maxUploads = Lude.Nothing,
      delimiter = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | Together with upload-id-marker, this parameter specifies the multipart upload after which listing should begin.
--
-- If @upload-id-marker@ is not specified, only the keys lexicographically greater than the specified @key-marker@ will be included in the list.
-- If @upload-id-marker@ is specified, any multipart uploads for a key equal to the @key-marker@ might also be included, provided those multipart uploads have upload IDs lexicographically greater than the specified @upload-id-marker@ .
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuKeyMarker :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuKeyMarker = Lens.lens (keyMarker :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListMultipartUploads)
{-# DEPRECATED lmuKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Lists in-progress uploads only for those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different grouping of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.)
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuPrefix :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuPrefix = Lens.lens (prefix :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListMultipartUploads)
{-# DEPRECATED lmuPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuEncodingType :: Lens.Lens' ListMultipartUploads (Lude.Maybe EncodingType)
lmuEncodingType = Lens.lens (encodingType :: ListMultipartUploads -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListMultipartUploads)
{-# DEPRECATED lmuEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Together with key-marker, specifies the multipart upload after which listing should begin. If key-marker is not specified, the upload-id-marker parameter is ignored. Otherwise, any multipart uploads for a key equal to the key-marker might be included in the list only if they have an upload ID lexicographically greater than the specified @upload-id-marker@ .
--
-- /Note:/ Consider using 'uploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuUploadIdMarker :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuUploadIdMarker = Lens.lens (uploadIdMarker :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {uploadIdMarker = a} :: ListMultipartUploads)
{-# DEPRECATED lmuUploadIdMarker "Use generic-lens or generic-optics with 'uploadIdMarker' instead." #-}

-- | Sets the maximum number of multipart uploads, from 1 to 1,000, to return in the response body. 1,000 is the maximum number of uploads that can be returned in a response.
--
-- /Note:/ Consider using 'maxUploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuMaxUploads :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Int)
lmuMaxUploads = Lens.lens (maxUploads :: ListMultipartUploads -> Lude.Maybe Lude.Int) (\s a -> s {maxUploads = a} :: ListMultipartUploads)
{-# DEPRECATED lmuMaxUploads "Use generic-lens or generic-optics with 'maxUploads' instead." #-}

-- | Character you use to group keys.
--
-- All keys that contain the same string between the prefix, if specified, and the first occurrence of the delimiter after the prefix are grouped under a single result element, @CommonPrefixes@ . If you don't specify the prefix parameter, then the substring starts at the beginning of the key. The keys that are grouped under @CommonPrefixes@ result element are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuDelimiter :: Lens.Lens' ListMultipartUploads (Lude.Maybe Delimiter)
lmuDelimiter = Lens.lens (delimiter :: ListMultipartUploads -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListMultipartUploads)
{-# DEPRECATED lmuDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuExpectedBucketOwner :: Lens.Lens' ListMultipartUploads (Lude.Maybe Lude.Text)
lmuExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListMultipartUploads -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListMultipartUploads)
{-# DEPRECATED lmuExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmuBucket :: Lens.Lens' ListMultipartUploads BucketName
lmuBucket = Lens.lens (bucket :: ListMultipartUploads -> BucketName) (\s a -> s {bucket = a} :: ListMultipartUploads)
{-# DEPRECATED lmuBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Page.AWSPager ListMultipartUploads where
  page rq rs
    | Page.stop (rs Lens.^. lmursIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lmursNextKeyMarker)
        Lude.&& Lude.isNothing (rs Lens.^. lmursNextUploadIdMarker) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmuKeyMarker Lens..~ rs Lens.^. lmursNextKeyMarker
          Lude.& lmuUploadIdMarker Lens..~ rs Lens.^. lmursNextUploadIdMarker

instance Lude.AWSRequest ListMultipartUploads where
  type Rs ListMultipartUploads = ListMultipartUploadsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListMultipartUploadsResponse'
            Lude.<$> (x Lude..@? "KeyMarker")
            Lude.<*> (x Lude..@? "Prefix")
            Lude.<*> (Lude.may (Lude.parseXMLList "CommonPrefixes") x)
            Lude.<*> (x Lude..@? "EncodingType")
            Lude.<*> (x Lude..@? "Bucket")
            Lude.<*> (x Lude..@? "UploadIdMarker")
            Lude.<*> (x Lude..@? "MaxUploads")
            Lude.<*> (x Lude..@? "NextKeyMarker")
            Lude.<*> (Lude.may (Lude.parseXMLList "Upload") x)
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (x Lude..@? "NextUploadIdMarker")
            Lude.<*> (x Lude..@? "Delimiter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMultipartUploads where
  toHeaders ListMultipartUploads' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath ListMultipartUploads where
  toPath ListMultipartUploads' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListMultipartUploads where
  toQuery ListMultipartUploads' {..} =
    Lude.mconcat
      [ "key-marker" Lude.=: keyMarker,
        "prefix" Lude.=: prefix,
        "encoding-type" Lude.=: encodingType,
        "upload-id-marker" Lude.=: uploadIdMarker,
        "max-uploads" Lude.=: maxUploads,
        "delimiter" Lude.=: delimiter,
        "uploads"
      ]

-- | /See:/ 'mkListMultipartUploadsResponse' smart constructor.
data ListMultipartUploadsResponse = ListMultipartUploadsResponse'
  { keyMarker ::
      Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    commonPrefixes ::
      Lude.Maybe [CommonPrefix],
    encodingType ::
      Lude.Maybe EncodingType,
    bucket :: Lude.Maybe BucketName,
    uploadIdMarker ::
      Lude.Maybe Lude.Text,
    maxUploads :: Lude.Maybe Lude.Int,
    nextKeyMarker ::
      Lude.Maybe Lude.Text,
    uploads ::
      Lude.Maybe [MultipartUpload],
    isTruncated ::
      Lude.Maybe Lude.Bool,
    nextUploadIdMarker ::
      Lude.Maybe Lude.Text,
    delimiter :: Lude.Maybe Delimiter,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMultipartUploadsResponse' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket to which the multipart upload was initiated.
-- * 'commonPrefixes' - If you specify a delimiter in the request, then the result returns each distinct key prefix containing the delimiter in a @CommonPrefixes@ element. The distinct key prefixes are returned in the @Prefix@ child element.
-- * 'delimiter' - Contains the delimiter you specified in the request. If you don't specify a delimiter in your request, this element is absent from the response.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter@ , @KeyMarker@ , @Prefix@ , @NextKeyMarker@ , @Key@ .
-- * 'isTruncated' - Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
-- * 'keyMarker' - The key at or after which the listing began.
-- * 'maxUploads' - Maximum number of multipart uploads that could have been included in the response.
-- * 'nextKeyMarker' - When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
-- * 'nextUploadIdMarker' - When a list is truncated, this element specifies the value that should be used for the @upload-id-marker@ request parameter in a subsequent request.
-- * 'prefix' - When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
-- * 'responseStatus' - The response status code.
-- * 'uploadIdMarker' - Upload ID after which listing began.
-- * 'uploads' - Container for elements related to a particular multipart upload. A response can contain zero or more @Upload@ elements.
mkListMultipartUploadsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMultipartUploadsResponse
mkListMultipartUploadsResponse pResponseStatus_ =
  ListMultipartUploadsResponse'
    { keyMarker = Lude.Nothing,
      prefix = Lude.Nothing,
      commonPrefixes = Lude.Nothing,
      encodingType = Lude.Nothing,
      bucket = Lude.Nothing,
      uploadIdMarker = Lude.Nothing,
      maxUploads = Lude.Nothing,
      nextKeyMarker = Lude.Nothing,
      uploads = Lude.Nothing,
      isTruncated = Lude.Nothing,
      nextUploadIdMarker = Lude.Nothing,
      delimiter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The key at or after which the listing began.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursKeyMarker = Lens.lens (keyMarker :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | When a prefix is provided in the request, this field contains the specified prefix. The result contains only keys starting with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursPrefix :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursPrefix = Lens.lens (prefix :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | If you specify a delimiter in the request, then the result returns each distinct key prefix containing the delimiter in a @CommonPrefixes@ element. The distinct key prefixes are returned in the @Prefix@ child element.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursCommonPrefixes :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe [CommonPrefix])
lmursCommonPrefixes = Lens.lens (commonPrefixes :: ListMultipartUploadsResponse -> Lude.Maybe [CommonPrefix]) (\s a -> s {commonPrefixes = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- If you specify @encoding-type@ request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @Delimiter@ , @KeyMarker@ , @Prefix@ , @NextKeyMarker@ , @Key@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursEncodingType :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe EncodingType)
lmursEncodingType = Lens.lens (encodingType :: ListMultipartUploadsResponse -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The name of the bucket to which the multipart upload was initiated.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursBucket :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe BucketName)
lmursBucket = Lens.lens (bucket :: ListMultipartUploadsResponse -> Lude.Maybe BucketName) (\s a -> s {bucket = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Upload ID after which listing began.
--
-- /Note:/ Consider using 'uploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursUploadIdMarker = Lens.lens (uploadIdMarker :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {uploadIdMarker = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursUploadIdMarker "Use generic-lens or generic-optics with 'uploadIdMarker' instead." #-}

-- | Maximum number of multipart uploads that could have been included in the response.
--
-- /Note:/ Consider using 'maxUploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursMaxUploads :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Int)
lmursMaxUploads = Lens.lens (maxUploads :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxUploads = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursMaxUploads "Use generic-lens or generic-optics with 'maxUploads' instead." #-}

-- | When a list is truncated, this element specifies the value that should be used for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursNextKeyMarker :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursNextKeyMarker = Lens.lens (nextKeyMarker :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextKeyMarker = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursNextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead." #-}

-- | Container for elements related to a particular multipart upload. A response can contain zero or more @Upload@ elements.
--
-- /Note:/ Consider using 'uploads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursUploads :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe [MultipartUpload])
lmursUploads = Lens.lens (uploads :: ListMultipartUploadsResponse -> Lude.Maybe [MultipartUpload]) (\s a -> s {uploads = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursUploads "Use generic-lens or generic-optics with 'uploads' instead." #-}

-- | Indicates whether the returned list of multipart uploads is truncated. A value of true indicates that the list was truncated. The list can be truncated if the number of multipart uploads exceeds the limit allowed or specified by max uploads.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursIsTruncated :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Bool)
lmursIsTruncated = Lens.lens (isTruncated :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | When a list is truncated, this element specifies the value that should be used for the @upload-id-marker@ request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextUploadIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursNextUploadIdMarker :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Lude.Text)
lmursNextUploadIdMarker = Lens.lens (nextUploadIdMarker :: ListMultipartUploadsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextUploadIdMarker = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursNextUploadIdMarker "Use generic-lens or generic-optics with 'nextUploadIdMarker' instead." #-}

-- | Contains the delimiter you specified in the request. If you don't specify a delimiter in your request, this element is absent from the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursDelimiter :: Lens.Lens' ListMultipartUploadsResponse (Lude.Maybe Delimiter)
lmursDelimiter = Lens.lens (delimiter :: ListMultipartUploadsResponse -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmursResponseStatus :: Lens.Lens' ListMultipartUploadsResponse Lude.Int
lmursResponseStatus = Lens.lens (responseStatus :: ListMultipartUploadsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMultipartUploadsResponse)
{-# DEPRECATED lmursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
