{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns some or all (up to 1,000) of the objects in a bucket. You can use the request parameters as selection criteria to return a subset of the objects in a bucket. A 200 OK response can contain valid or invalid XML. Be sure to design your application to parse the contents of the response and handle it appropriately.
--
-- /Important:/ This API has been revised. We recommend that you use the newer version, <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjectsV2.html ListObjectsV2> , when developing applications. For backward compatibility, Amazon S3 continues to support @ListObjects@ .
-- The following operations are related to @ListObjects@ :
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
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html ListBuckets>
--
--
--
-- This operation returns paginated results.
module Network.AWS.S3.ListObjects
  ( -- * Creating a request
    ListObjects (..),
    mkListObjects,

    -- ** Request lenses
    loPrefix,
    loEncodingType,
    loBucket,
    loRequestPayer,
    loMarker,
    loMaxKeys,
    loDelimiter,
    loExpectedBucketOwner,

    -- * Destructuring the response
    ListObjectsResponse (..),
    mkListObjectsResponse,

    -- ** Response lenses
    lorsContents,
    lorsPrefix,
    lorsCommonPrefixes,
    lorsEncodingType,
    lorsName,
    lorsMarker,
    lorsNextMarker,
    lorsMaxKeys,
    lorsIsTruncated,
    lorsDelimiter,
    lorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkListObjects' smart constructor.
data ListObjects = ListObjects'
  { -- | Limits the response to keys that begin with the specified prefix.
    prefix :: Lude.Maybe Lude.Text,
    encodingType :: Lude.Maybe EncodingType,
    -- | The name of the bucket containing the objects.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: BucketName,
    -- | Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
    requestPayer :: Lude.Maybe RequestPayer,
    -- | Specifies the key to start with when listing objects in a bucket.
    marker :: Lude.Maybe Lude.Text,
    -- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
    maxKeys :: Lude.Maybe Lude.Int,
    -- | A delimiter is a character you use to group keys.
    delimiter :: Lude.Maybe Delimiter,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjects' with the minimum fields required to make a request.
--
-- * 'prefix' - Limits the response to keys that begin with the specified prefix.
-- * 'encodingType' -
-- * 'bucket' - The name of the bucket containing the objects.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'requestPayer' - Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
-- * 'marker' - Specifies the key to start with when listing objects in a bucket.
-- * 'maxKeys' - Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
-- * 'delimiter' - A delimiter is a character you use to group keys.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkListObjects ::
  -- | 'bucket'
  BucketName ->
  ListObjects
mkListObjects pBucket_ =
  ListObjects'
    { prefix = Lude.Nothing,
      encodingType = Lude.Nothing,
      bucket = pBucket_,
      requestPayer = Lude.Nothing,
      marker = Lude.Nothing,
      maxKeys = Lude.Nothing,
      delimiter = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Limits the response to keys that begin with the specified prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loPrefix :: Lens.Lens' ListObjects (Lude.Maybe Lude.Text)
loPrefix = Lens.lens (prefix :: ListObjects -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjects)
{-# DEPRECATED loPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loEncodingType :: Lens.Lens' ListObjects (Lude.Maybe EncodingType)
loEncodingType = Lens.lens (encodingType :: ListObjects -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjects)
{-# DEPRECATED loEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The name of the bucket containing the objects.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loBucket :: Lens.Lens' ListObjects BucketName
loBucket = Lens.lens (bucket :: ListObjects -> BucketName) (\s a -> s {bucket = a} :: ListObjects)
{-# DEPRECATED loBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Confirms that the requester knows that she or he will be charged for the list objects request. Bucket owners need not specify this parameter in their requests.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRequestPayer :: Lens.Lens' ListObjects (Lude.Maybe RequestPayer)
loRequestPayer = Lens.lens (requestPayer :: ListObjects -> Lude.Maybe RequestPayer) (\s a -> s {requestPayer = a} :: ListObjects)
{-# DEPRECATED loRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

-- | Specifies the key to start with when listing objects in a bucket.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMarker :: Lens.Lens' ListObjects (Lude.Maybe Lude.Text)
loMarker = Lens.lens (marker :: ListObjects -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListObjects)
{-# DEPRECATED loMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Sets the maximum number of keys returned in the response. By default the API returns up to 1,000 key names. The response might contain fewer keys but will never contain more.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxKeys :: Lens.Lens' ListObjects (Lude.Maybe Lude.Int)
loMaxKeys = Lens.lens (maxKeys :: ListObjects -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjects)
{-# DEPRECATED loMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A delimiter is a character you use to group keys.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDelimiter :: Lens.Lens' ListObjects (Lude.Maybe Delimiter)
loDelimiter = Lens.lens (delimiter :: ListObjects -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjects)
{-# DEPRECATED loDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loExpectedBucketOwner :: Lens.Lens' ListObjects (Lude.Maybe Lude.Text)
loExpectedBucketOwner = Lens.lens (expectedBucketOwner :: ListObjects -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: ListObjects)
{-# DEPRECATED loExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Page.AWSPager ListObjects where
  page rq rs
    | Page.stop (rs Lens.^. lorsIsTruncated) = Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^. Lude.choice
              (Lens.^. lorsNextMarker)
              (Lens.^? (lorsContents Lude.. Lens._last Lude.. oKey))
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loMarker
          Lens..~ rs
          Lens.^. Lude.choice
            (Lens.^. lorsNextMarker)
            (Lens.^? (lorsContents Lude.. Lens._last Lude.. oKey))

instance Lude.AWSRequest ListObjects where
  type Rs ListObjects = ListObjectsResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListObjectsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "Contents") x)
            Lude.<*> (x Lude..@? "Prefix")
            Lude.<*> (Lude.may (Lude.parseXMLList "CommonPrefixes") x)
            Lude.<*> (x Lude..@? "EncodingType")
            Lude.<*> (x Lude..@? "Name")
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (x Lude..@? "MaxKeys")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (x Lude..@? "Delimiter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjects where
  toHeaders ListObjects' {..} =
    Lude.mconcat
      [ "x-amz-request-payer" Lude.=# requestPayer,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath ListObjects where
  toPath ListObjects' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery ListObjects where
  toQuery ListObjects' {..} =
    Lude.mconcat
      [ "prefix" Lude.=: prefix,
        "encoding-type" Lude.=: encodingType,
        "marker" Lude.=: marker,
        "max-keys" Lude.=: maxKeys,
        "delimiter" Lude.=: delimiter
      ]

-- | /See:/ 'mkListObjectsResponse' smart constructor.
data ListObjectsResponse = ListObjectsResponse'
  { -- | Metadata about each object returned.
    contents :: Lude.Maybe [Object],
    -- | Keys that begin with the indicated prefix.
    prefix :: Lude.Maybe Lude.Text,
    -- | All of the keys rolled up in a common prefix count as a single return when calculating the number of returns.
    --
    -- A response can contain CommonPrefixes only if you specify a delimiter.
    -- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by the delimiter.
    -- CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix.
    -- For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
    commonPrefixes :: Lude.Maybe [CommonPrefix],
    -- | Encoding type used by Amazon S3 to encode object keys in the response.
    encodingType :: Lude.Maybe EncodingType,
    -- | The bucket name.
    name :: Lude.Maybe BucketName,
    -- | Indicates where in the bucket listing begins. Marker is included in the response if it was sent with the request.
    marker :: Lude.Maybe Lude.Text,
    -- | When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMarker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The maximum number of keys returned in the response body.
    maxKeys :: Lude.Maybe Lude.Int,
    -- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the @CommonPrefixes@ collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
    delimiter :: Lude.Maybe Delimiter,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectsResponse' with the minimum fields required to make a request.
--
-- * 'contents' - Metadata about each object returned.
-- * 'prefix' - Keys that begin with the indicated prefix.
-- * 'commonPrefixes' - All of the keys rolled up in a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain CommonPrefixes only if you specify a delimiter.
-- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by the delimiter.
-- CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix.
-- For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
-- * 'encodingType' - Encoding type used by Amazon S3 to encode object keys in the response.
-- * 'name' - The bucket name.
-- * 'marker' - Indicates where in the bucket listing begins. Marker is included in the response if it was sent with the request.
-- * 'nextMarker' - When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMarker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
-- * 'maxKeys' - The maximum number of keys returned in the response body.
-- * 'isTruncated' - A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria.
-- * 'delimiter' - Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the @CommonPrefixes@ collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
-- * 'responseStatus' - The response status code.
mkListObjectsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectsResponse
mkListObjectsResponse pResponseStatus_ =
  ListObjectsResponse'
    { contents = Lude.Nothing,
      prefix = Lude.Nothing,
      commonPrefixes = Lude.Nothing,
      encodingType = Lude.Nothing,
      name = Lude.Nothing,
      marker = Lude.Nothing,
      nextMarker = Lude.Nothing,
      maxKeys = Lude.Nothing,
      isTruncated = Lude.Nothing,
      delimiter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Metadata about each object returned.
--
-- /Note:/ Consider using 'contents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsContents :: Lens.Lens' ListObjectsResponse (Lude.Maybe [Object])
lorsContents = Lens.lens (contents :: ListObjectsResponse -> Lude.Maybe [Object]) (\s a -> s {contents = a} :: ListObjectsResponse)
{-# DEPRECATED lorsContents "Use generic-lens or generic-optics with 'contents' instead." #-}

-- | Keys that begin with the indicated prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsPrefix :: Lens.Lens' ListObjectsResponse (Lude.Maybe Lude.Text)
lorsPrefix = Lens.lens (prefix :: ListObjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListObjectsResponse)
{-# DEPRECATED lorsPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of the keys rolled up in a common prefix count as a single return when calculating the number of returns.
--
-- A response can contain CommonPrefixes only if you specify a delimiter.
-- CommonPrefixes contains all (if there are any) keys between Prefix and the next occurrence of the string specified by the delimiter.
-- CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix.
-- For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. All of the keys that roll up into a common prefix count as a single return when calculating the number of returns.
--
-- /Note:/ Consider using 'commonPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsCommonPrefixes :: Lens.Lens' ListObjectsResponse (Lude.Maybe [CommonPrefix])
lorsCommonPrefixes = Lens.lens (commonPrefixes :: ListObjectsResponse -> Lude.Maybe [CommonPrefix]) (\s a -> s {commonPrefixes = a} :: ListObjectsResponse)
{-# DEPRECATED lorsCommonPrefixes "Use generic-lens or generic-optics with 'commonPrefixes' instead." #-}

-- | Encoding type used by Amazon S3 to encode object keys in the response.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsEncodingType :: Lens.Lens' ListObjectsResponse (Lude.Maybe EncodingType)
lorsEncodingType = Lens.lens (encodingType :: ListObjectsResponse -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: ListObjectsResponse)
{-# DEPRECATED lorsEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsName :: Lens.Lens' ListObjectsResponse (Lude.Maybe BucketName)
lorsName = Lens.lens (name :: ListObjectsResponse -> Lude.Maybe BucketName) (\s a -> s {name = a} :: ListObjectsResponse)
{-# DEPRECATED lorsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Indicates where in the bucket listing begins. Marker is included in the response if it was sent with the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsMarker :: Lens.Lens' ListObjectsResponse (Lude.Maybe Lude.Text)
lorsMarker = Lens.lens (marker :: ListObjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListObjectsResponse)
{-# DEPRECATED lorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | When response is truncated (the IsTruncated element value in the response is true), you can use the key name in this field as marker in the subsequent request to get next set of objects. Amazon S3 lists objects in alphabetical order Note: This element is returned only if you have delimiter request parameter specified. If response does not include the NextMarker and it is truncated, you can use the value of the last Key in the response as the marker in the subsequent request to get the next set of object keys.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsNextMarker :: Lens.Lens' ListObjectsResponse (Lude.Maybe Lude.Text)
lorsNextMarker = Lens.lens (nextMarker :: ListObjectsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListObjectsResponse)
{-# DEPRECATED lorsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The maximum number of keys returned in the response body.
--
-- /Note:/ Consider using 'maxKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsMaxKeys :: Lens.Lens' ListObjectsResponse (Lude.Maybe Lude.Int)
lorsMaxKeys = Lens.lens (maxKeys :: ListObjectsResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxKeys = a} :: ListObjectsResponse)
{-# DEPRECATED lorsMaxKeys "Use generic-lens or generic-optics with 'maxKeys' instead." #-}

-- | A flag that indicates whether Amazon S3 returned all of the results that satisfied the search criteria.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsIsTruncated :: Lens.Lens' ListObjectsResponse (Lude.Maybe Lude.Bool)
lorsIsTruncated = Lens.lens (isTruncated :: ListObjectsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListObjectsResponse)
{-# DEPRECATED lorsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | Causes keys that contain the same string between the prefix and the first occurrence of the delimiter to be rolled up into a single result element in the @CommonPrefixes@ collection. These rolled-up keys are not returned elsewhere in the response. Each rolled-up result counts as only one return against the @MaxKeys@ value.
--
-- /Note:/ Consider using 'delimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsDelimiter :: Lens.Lens' ListObjectsResponse (Lude.Maybe Delimiter)
lorsDelimiter = Lens.lens (delimiter :: ListObjectsResponse -> Lude.Maybe Delimiter) (\s a -> s {delimiter = a} :: ListObjectsResponse)
{-# DEPRECATED lorsDelimiter "Use generic-lens or generic-optics with 'delimiter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorsResponseStatus :: Lens.Lens' ListObjectsResponse Lude.Int
lorsResponseStatus = Lens.lens (responseStatus :: ListObjectsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectsResponse)
{-# DEPRECATED lorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
