{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about all versions of the objects in a bucket. You can also use request parameters as selection criteria to return metadata about a subset of all the object versions.
--
-- To use this operation, you must have READ access to the bucket.
-- This action is not supported by Amazon S3 on Outposts.
-- The following operations are related to @ListObjectVersions@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjectVersions
  ( -- * Creating a request
    ListObjectVersions (..),
    mkListObjectVersions,

    -- ** Request lenses
    lKeyMarker,
    lPrefix,
    lEncodingType,
    lVersionIdMarker,
    lMaxKeys,
    lDelimiter,
    lExpectedBucketOwner,
    lBucket,

    -- * Destructuring the response
    ListObjectVersionsResponse (..),
    mkListObjectVersionsResponse,

    -- ** Response lenses
    lrsNextVersionIdMarker,
    lrsKeyMarker,
    lrsDeleteMarkers,
    lrsPrefix,
    lrsCommonPrefixes,
    lrsEncodingType,
    lrsVersions,
    lrsName,
    lrsNextKeyMarker,
    lrsVersionIdMarker,
    lrsMaxKeys,
    lrsIsTruncated,
    lrsDelimiter,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListObjectVersions' smart constructor.
data ListObjectVersions = ListObjectVersions'
  { keyMarker ::
      Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    encodingType :: Lude.Maybe EncodingType,
    versionIdMarker :: Lude.Maybe Lude.Text,
    maxKeys :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'ListObjectVersions' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name that contains the objects.
-- * 'delimiter' - A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
-- * 'encodingType' - Undocumented field.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'keyMarker' - Specifies the key to start with when listing objects in a bucket.
-- * 'maxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
-- * 'prefix' - Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
-- * 'versionIdMarker' - Specifies the object version you want to start listing from.
mkListObjectVersions ::
  -- | 'bucket'
  BucketName ->
  ListObjectVersions
mkListObjectVersions pBucket_ =
  ListObjectVersions'
    { keyMarker = Lude.Nothing,
      prefix = Lude.Nothing,
      encodingType = Lude.Nothing,
      versionIdMarker = Lude.Nothing,
      maxKeys = Lude.Nothing,
      delimiter = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lKeyMarker :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lKeyMarker = Lens.lens (keyMarker :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListObjectVersions)
{-# DEPRECATED lKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPrefix :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lPrefix = Lens.lens (prefix :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectVersions)
{-# DEPRECATED lPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lEncodingType :: Lens.Lens' ListObjectVersions (Lude.Maybe EncodingType)
lEncodingType = Lens.lens (encodingType :: ListObjectVersions -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectVersions)
{-# DEPRECATED lEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Specifies the object version you want to start listing from.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lVersionIdMarker :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lVersionIdMarker = Lens.lens (versionIdMarker :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {versionIdMarker = a} :: ListObjectVersions)
{-# DEPRECATED lVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxKeys :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Int)
lMaxKeys = Lens.lens (maxKeys :: ListObjectVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectVersions)
{-# DEPRECATED lMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDelimiter :: Lens.Lens' ListObjectVersions (Lude.Maybe Delimiter)
lDelimiter = Lens.lens (delimiter :: ListObjectVersions -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectVersions)
{-# DEPRECATED lDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lExpectedBucketOwner :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListObjectVersions)
{-# DEPRECATED lExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name that contains the objects.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lBucket :: Lens.Lens' ListObjectVersions BucketName
lBucket = Lens.lens (bucket :: ListObjectVersions -> BucketName) (\s a -> s {bucket = a} :: ListObjectVersions)
{-# DEPRECATED lBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Page.AWSPager ListObjectVersions where
  page rq rs
    | Page.stop (rs Lens.^. lrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lrsNextKeyMarker)
        Lude.&& Lude.isNothing (rs Lens.^. lrsNextVersionIdMarker) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lKeyMarker Lens..~ rs Lens.^. lrsNextKeyMarker
          Lude.& lVersionIdMarker Lens..~ rs Lens.^. lrsNextVersionIdMarker

instance Lude.AWSRequest ListObjectVersions where
  type Rs ListObjectVersions = ListObjectVersionsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListObjectVersionsResponse'
            Lude.<$> (x Lude..@? "NextVersionIdMarker")
            Lude.<*> (x Lude..@? "KeyMarker")
            Lude.<*> (Lude.may (Lude.parseXMLList "DeleteMarker") x)
            Lude.<*> (x Lude..@? "Prefix")
            Lude.<*> (Lude.may (Lude.parseXMLList "CommonPrefixes") x)
            Lude.<*> (x Lude..@? "EncodingType")
            Lude.<*> (Lude.may (Lude.parseXMLList "Version") x)
            Lude.<*> (x Lude..@? "Name")
            Lude.<*> (x Lude..@? "NextKeyMarker")
            Lude.<*> (x Lude..@? "VersionIdMarker")
            Lude.<*> (x Lude..@? "MaxKeys")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (x Lude..@? "Delimiter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectVersions where
  toHeaders ListObjectVersions' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath ListObjectVersions where
  toPath ListObjectVersions' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListObjectVersions where
  toQuery ListObjectVersions' {..} =
    Lude.mconcat
      [ "key-marker" Lude.=: keyMarker,
        "prefix" Lude.=: prefix,
        "encoding-type" Lude.=: encodingType,
        "version-id-marker" Lude.=: versionIdMarker,
        "max-keys" Lude.=: maxKeys,
        "delimiter" Lude.=: delimiter,
        "versions"
      ]

-- | /See:/ 'mkListObjectVersionsResponse' smart constructor.
data ListObjectVersionsResponse = ListObjectVersionsResponse'
  { nextVersionIdMarker ::
      Lude.Maybe Lude.Text,
    keyMarker :: Lude.Maybe Lude.Text,
    deleteMarkers ::
      Lude.Maybe [DeleteMarkerEntry],
    prefix :: Lude.Maybe Lude.Text,
    commonPrefixes ::
      Lude.Maybe [CommonPrefix],
    encodingType ::
      Lude.Maybe EncodingType,
    versions ::
      Lude.Maybe [ObjectVersion],
    name :: Lude.Maybe BucketName,
    nextKeyMarker :: Lude.Maybe Lude.Text,
    versionIdMarker ::
      Lude.Maybe Lude.Text,
    maxKeys :: Lude.Maybe Lude.Int,
    isTruncated :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'ListObjectVersionsResponse' with the minimum fields required to make a request.
--
-- * 'commonPrefixes' - All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
-- * 'deleteMarkers' - Container for an object that is a delete marker.
-- * 'delimiter' - The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
-- * 'isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
-- * 'keyMarker' - Marks the last key returned in a truncated response.
-- * 'maxKeys' - Specifies the maximum number of objects to return.
-- * 'name' - The bucket name.
-- * 'nextKeyMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
-- * 'nextVersionIdMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
-- * 'prefix' - Selects objects that start with the value supplied by this parameter.
-- * 'responseStatus' - The response status code.
-- * 'versionIdMarker' - Marks the last version of the key returned in a truncated response.
-- * 'versions' - Container for version information.
mkListObjectVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectVersionsResponse
mkListObjectVersionsResponse pResponseStatus_ =
  ListObjectVersionsResponse'
    { nextVersionIdMarker = Lude.Nothing,
      keyMarker = Lude.Nothing,
      deleteMarkers = Lude.Nothing,
      prefix = Lude.Nothing,
      commonPrefixes = Lude.Nothing,
      encodingType = Lude.Nothing,
      versions = Lude.Nothing,
      name = Lude.Nothing,
      nextKeyMarker = Lude.Nothing,
      versionIdMarker = Lude.Nothing,
      maxKeys = Lude.Nothing,
      isTruncated = Lude.Nothing,
      delimiter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextVersionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lrsNextVersionIdMarker = Lens.lens (nextVersionIdMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextVersionIdMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsNextVersionIdMarker "Use generic-lens or generic-optics with 'nextVersionIdMarker' instead." #-}

-- | Marks the last key returned in a truncated response.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lrsKeyMarker = Lens.lens (keyMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Container for an object that is a delete marker.
--
-- /Note:/ Consider using 'deleteMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDeleteMarkers :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [DeleteMarkerEntry])
lrsDeleteMarkers = Lens.lens (deleteMarkers :: ListObjectVersionsResponse -> Lude.Maybe [DeleteMarkerEntry]) (\s a -> s {deleteMarkers = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsDeleteMarkers "Use generic-lens or generic-optics with 'deleteMarkers' instead." #-}

-- | Selects objects that start with the value supplied by this parameter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsPrefix :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lrsPrefix = Lens.lens (prefix :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsCommonPrefixes :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [CommonPrefix])
lrsCommonPrefixes = Lens.lens (commonPrefixes :: ListObjectVersionsResponse -> Lude.Maybe [CommonPrefix]) (\s a -> s {commonPrefixes = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsEncodingType :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe EncodingType)
lrsEncodingType = Lens.lens (encodingType :: ListObjectVersionsResponse -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Container for version information.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersions :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [ObjectVersion])
lrsVersions = Lens.lens (versions :: ListObjectVersionsResponse -> Lude.Maybe [ObjectVersion]) (\s a -> s {versions = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsName :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe BucketName)
lrsName = Lens.lens (name :: ListObjectVersionsResponse -> Lude.Maybe BucketName) (\s a -> s {name = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lrsNextKeyMarker = Lens.lens (nextKeyMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextKeyMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsNextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead." #-}

-- | Marks the last version of the key returned in a truncated response.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lrsVersionIdMarker = Lens.lens (versionIdMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionIdMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

-- | Specifies the maximum number of objects to return.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsMaxKeys :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Int)
lrsMaxKeys = Lens.lens (maxKeys :: ListObjectVersionsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsIsTruncated :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Bool)
lrsIsTruncated = Lens.lens (isTruncated :: ListObjectVersionsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDelimiter :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Delimiter)
lrsDelimiter = Lens.lens (delimiter :: ListObjectVersionsResponse -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListObjectVersionsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListObjectVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
