{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lovKeyMarker,
    lovPrefix,
    lovEncodingType,
    lovBucket,
    lovVersionIdMarker,
    lovMaxKeys,
    lovDelimiter,
    lovExpectedBucketOwner,

    -- * Destructuring the response
    ListObjectVersionsResponse (..),
    mkListObjectVersionsResponse,

    -- ** Response lenses
    lovrsNextVersionIdMarker,
    lovrsKeyMarker,
    lovrsDeleteMarkers,
    lovrsPrefix,
    lovrsCommonPrefixes,
    lovrsEncodingType,
    lovrsVersions,
    lovrsName,
    lovrsNextKeyMarker,
    lovrsVersionIdMarker,
    lovrsMaxKeys,
    lovrsIsTruncated,
    lovrsDelimiter,
    lovrsResponseStatus,
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
  { -- | Specifies the key to start with when listing objects in a bucket.
    keyMarker :: Lude.Maybe Lude.Text,
    -- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
    prefix :: Lude.Maybe Lude.Text,
    encodingType :: Lude.Maybe EncodingType,
    -- | The bucket name that contains the objects.
    bucket :: BucketName,
    -- | Specifies the object version you want to start listing from.
    versionIdMarker :: Lude.Maybe Lude.Text,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
    maxKeys :: Lude.Maybe Lude.Int,
    -- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
    delimiter :: Lude.Maybe Delimiter,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectVersions' with the minimum fields required to make a request.
--
-- * 'keyMarker' - Specifies the key to start with when listing objects in a bucket.
-- * 'prefix' - Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
-- * 'encodingType' -
-- * 'bucket' - The bucket name that contains the objects.
-- * 'versionIdMarker' - Specifies the object version you want to start listing from.
-- * 'maxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
-- * 'delimiter' - A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListObjectVersions ::
  -- | 'bucket'
  BucketName ->
  ListObjectVersions
mkListObjectVersions pBucket_ =
  ListObjectVersions'
    { keyMarker = Lude.Nothing,
      prefix = Lude.Nothing,
      encodingType = Lude.Nothing,
      bucket = pBucket_,
      versionIdMarker = Lude.Nothing,
      maxKeys = Lude.Nothing,
      delimiter = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovKeyMarker :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lovKeyMarker = Lens.lens (keyMarker :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListObjectVersions)
{-# DEPRECATED lovKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Use this parameter to select only those keys that begin with the specified prefix. You can use prefixes to separate a bucket into different groupings of keys. (You can think of using prefix to make groups in the same way you'd use a folder in a file system.) You can use prefix with delimiter to roll up numerous objects into a single result under CommonPrefixes.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovPrefix :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lovPrefix = Lens.lens (prefix :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectVersions)
{-# DEPRECATED lovPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovEncodingType :: Lens.Lens' ListObjectVersions (Lude.Maybe EncodingType)
lovEncodingType = Lens.lens (encodingType :: ListObjectVersions -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectVersions)
{-# DEPRECATED lovEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The bucket name that contains the objects.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovBucket :: Lens.Lens' ListObjectVersions BucketName
lovBucket = Lens.lens (bucket :: ListObjectVersions -> BucketName) (\s a -> s {bucket = a} :: ListObjectVersions)
{-# DEPRECATED lovBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specifies the object version you want to start listing from.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovVersionIdMarker :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lovVersionIdMarker = Lens.lens (versionIdMarker :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {versionIdMarker = a} :: ListObjectVersions)
{-# DEPRECATED lovVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more. If additional keys satisfy the search criteria, but were not returned because max-keys was exceeded, the response contains <isTruncated>true</isTruncated>. To return the additional keys, see key-marker and version-id-marker.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovMaxKeys :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Int)
lovMaxKeys = Lens.lens (maxKeys :: ListObjectVersions -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectVersions)
{-# DEPRECATED lovMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A delimiter is a character that you specify to group keys. All keys that contain the same string between the @prefix@ and the first occurrence of the delimiter are grouped under a single result element in CommonPrefixes. These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovDelimiter :: Lens.Lens' ListObjectVersions (Lude.Maybe Delimiter)
lovDelimiter = Lens.lens (delimiter :: ListObjectVersions -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectVersions)
{-# DEPRECATED lovDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovExpectedBucketOwner :: Lens.Lens' ListObjectVersions (Lude.Maybe Lude.Text)
lovExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListObjectVersions -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListObjectVersions)
{-# DEPRECATED lovExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Page.AWSPager ListObjectVersions where
  page rq rs
    | Page.stop (rs Lens.^. lovrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lovrsNextKeyMarker)
        Lude.&& Lude.isNothing (rs Lens.^. lovrsNextVersionIdMarker) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lovKeyMarker Lens..~ rs Lens.^. lovrsNextKeyMarker
          Lude.& lovVersionIdMarker Lens..~ rs Lens.^. lovrsNextVersionIdMarker

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
  { -- | When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
    nextVersionIdMarker :: Lude.Maybe Lude.Text,
    -- | Marks the last key returned in a truncated response.
    keyMarker :: Lude.Maybe Lude.Text,
    -- | Container for an object that is a delete marker.
    deleteMarkers :: Lude.Maybe [DeleteMarkerEntry],
    -- | Selects objects that start with the value supplied by this parameter.
    prefix :: Lude.Maybe Lude.Text,
    -- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
    commonPrefixes :: Lude.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object key names in the XML response.
    --
    -- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
    -- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
    encodingType :: Lude.Maybe EncodingType,
    -- | Container for version information.
    versions :: Lude.Maybe [ObjectVersion],
    -- | The bucket name.
    name :: Lude.Maybe BucketName,
    -- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
    nextKeyMarker :: Lude.Maybe Lude.Text,
    -- | Marks the last version of the key returned in a truncated response.
    versionIdMarker :: Lude.Maybe Lude.Text,
    -- | Specifies the maximum number of objects to return.
    maxKeys :: Lude.Maybe Lude.Int,
    -- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
    delimiter :: Lude.Maybe Delimiter,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextVersionIdMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextVersionIdMarker@ specifies the first object version not returned that satisfies the search criteria. Use this value for the version-id-marker request parameter in a subsequent request.
-- * 'keyMarker' - Marks the last key returned in a truncated response.
-- * 'deleteMarkers' - Container for an object that is a delete marker.
-- * 'prefix' - Selects objects that start with the value supplied by this parameter.
-- * 'commonPrefixes' - All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
-- * 'versions' - Container for version information.
-- * 'name' - The bucket name.
-- * 'nextKeyMarker' - When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
-- * 'versionIdMarker' - Marks the last version of the key returned in a truncated response.
-- * 'maxKeys' - Specifies the maximum number of objects to return.
-- * 'isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
-- * 'delimiter' - The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
-- * 'responseStatus' - The response status code.
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
lovrsNextVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lovrsNextVersionIdMarker = Lens.lens (nextVersionIdMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextVersionIdMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsNextVersionIdMarker "Use generic-lens or generic-optics with 'nextVersionIdMarker' instead." #-}

-- | Marks the last key returned in a truncated response.
--
-- /Note:/ Consider using 'keyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lovrsKeyMarker = Lens.lens (keyMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {keyMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsKeyMarker "Use generic-lens or generic-optics with 'keyMarker' instead." #-}

-- | Container for an object that is a delete marker.
--
-- /Note:/ Consider using 'deleteMarkers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsDeleteMarkers :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [DeleteMarkerEntry])
lovrsDeleteMarkers = Lens.lens (deleteMarkers :: ListObjectVersionsResponse -> Lude.Maybe [DeleteMarkerEntry]) (\s a -> s {deleteMarkers = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsDeleteMarkers "Use generic-lens or generic-optics with 'deleteMarkers' instead." #-}

-- | Selects objects that start with the value supplied by this parameter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsPrefix :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lovrsPrefix = Lens.lens (prefix :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of the keys rolled up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsCommonPrefixes :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [CommonPrefix])
lovrsCommonPrefixes = Lens.lens (commonPrefixes :: ListObjectVersionsResponse -> Lude.Maybe [CommonPrefix]) (\s a -> s {commonPrefixes = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Encoding type used by Amazon S3 to encode object key names in the XML response.
--
-- If you specify encoding-type request parameter, Amazon S3 includes this element in the response, and returns encoded key name values in the following response elements:
-- @KeyMarker, NextKeyMarker, Prefix, Key@ , and @Delimiter@ .
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsEncodingType :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe EncodingType)
lovrsEncodingType = Lens.lens (encodingType :: ListObjectVersionsResponse -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Container for version information.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsVersions :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe [ObjectVersion])
lovrsVersions = Lens.lens (versions :: ListObjectVersionsResponse -> Lude.Maybe [ObjectVersion]) (\s a -> s {versions = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsName :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe BucketName)
lovrsName = Lens.lens (name :: ListObjectVersionsResponse -> Lude.Maybe BucketName) (\s a -> s {name = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When the number of responses exceeds the value of @MaxKeys@ , @NextKeyMarker@ specifies the first key not returned that satisfies the search criteria. Use this value for the key-marker request parameter in a subsequent request.
--
-- /Note:/ Consider using 'nextKeyMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsNextKeyMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lovrsNextKeyMarker = Lens.lens (nextKeyMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextKeyMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsNextKeyMarker "Use generic-lens or generic-optics with 'nextKeyMarker' instead." #-}

-- | Marks the last version of the key returned in a truncated response.
--
-- /Note:/ Consider using 'versionIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsVersionIdMarker :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Text)
lovrsVersionIdMarker = Lens.lens (versionIdMarker :: ListObjectVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionIdMarker = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsVersionIdMarker "Use generic-lens or generic-optics with 'versionIdMarker' instead." #-}

-- | Specifies the maximum number of objects to return.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsMaxKeys :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Int)
lovrsMaxKeys = Lens.lens (maxKeys :: ListObjectVersionsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria. If your results were truncated, you can make a follow-up paginated request using the NextKeyMarker and NextVersionIdMarker response parameters as a starting place in another request to return the rest of the results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsIsTruncated :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Lude.Bool)
lovrsIsTruncated = Lens.lens (isTruncated :: ListObjectVersionsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The delimiter grouping the included keys. A delimiter is a character that you specify to group keys. All keys that contain the same string between the prefix and the first occurrence of the delimiter are grouped under a single result element in @CommonPrefixes@ . These groups are counted as one result against the max-keys limitation. These keys are not returned elsewhere in the response.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsDelimiter :: Lens.Lens' ListObjectVersionsResponse (Lude.Maybe Delimiter)
lovrsDelimiter = Lens.lens (delimiter :: ListObjectVersionsResponse -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lovrsResponseStatus :: Lens.Lens' ListObjectVersionsResponse Lude.Int
lovrsResponseStatus = Lens.lens (responseStatus :: ListObjectVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectVersionsResponse)
{-# DEPRECATED lovrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
