{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehavior where

import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.TrustedKeyGroups
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that describes how CloudFront processes requests.
--
--
-- You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to serve objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.
--
-- For the current quota (formerly known as limit) on the number of cache behaviors that you can add to a distribution, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> in the /Amazon CloudFront Developer Guide/ .
--
-- If you don’t want to specify any cache behaviors, include only an empty @CacheBehaviors@ element. Don’t include an empty @CacheBehavior@ element because this is invalid.
--
-- To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty @CacheBehaviors@ element.
--
-- To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.
--
-- For more information about cache behaviors, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'cacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { _cbAllowedMethods ::
      !(Maybe AllowedMethods),
    _cbLambdaFunctionAssociations ::
      !(Maybe LambdaFunctionAssociations),
    _cbMaxTTL :: !(Maybe Integer),
    _cbMinTTL :: !(Maybe Integer),
    _cbCompress :: !(Maybe Bool),
    _cbSmoothStreaming :: !(Maybe Bool),
    _cbTrustedKeyGroups :: !(Maybe TrustedKeyGroups),
    _cbRealtimeLogConfigARN :: !(Maybe Text),
    _cbDefaultTTL :: !(Maybe Integer),
    _cbForwardedValues :: !(Maybe ForwardedValues),
    _cbTrustedSigners :: !(Maybe TrustedSigners),
    _cbOriginRequestPolicyId :: !(Maybe Text),
    _cbFieldLevelEncryptionId :: !(Maybe Text),
    _cbCachePolicyId :: !(Maybe Text),
    _cbPathPattern :: !Text,
    _cbTargetOriginId :: !Text,
    _cbViewerProtocolPolicy :: !ViewerProtocolPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbAllowedMethods' - Undocumented member.
--
-- * 'cbLambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- * 'cbMaxTTL' - This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbMinTTL' - This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
--
-- * 'cbCompress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbSmoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- * 'cbTrustedKeyGroups' - A list of key groups that CloudFront can use to validate signed URLs or signed cookies. When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbRealtimeLogConfigARN' - The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbDefaultTTL' - This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbForwardedValues' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ . If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- * 'cbTrustedSigners' - /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ . A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies. When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbOriginRequestPolicyId' - The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbFieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
--
-- * 'cbCachePolicyId' - The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbPathPattern' - The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbTargetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
--
-- * 'cbViewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
cacheBehavior ::
  -- | 'cbPathPattern'
  Text ->
  -- | 'cbTargetOriginId'
  Text ->
  -- | 'cbViewerProtocolPolicy'
  ViewerProtocolPolicy ->
  CacheBehavior
cacheBehavior pPathPattern_ pTargetOriginId_ pViewerProtocolPolicy_ =
  CacheBehavior'
    { _cbAllowedMethods = Nothing,
      _cbLambdaFunctionAssociations = Nothing,
      _cbMaxTTL = Nothing,
      _cbMinTTL = Nothing,
      _cbCompress = Nothing,
      _cbSmoothStreaming = Nothing,
      _cbTrustedKeyGroups = Nothing,
      _cbRealtimeLogConfigARN = Nothing,
      _cbDefaultTTL = Nothing,
      _cbForwardedValues = Nothing,
      _cbTrustedSigners = Nothing,
      _cbOriginRequestPolicyId = Nothing,
      _cbFieldLevelEncryptionId = Nothing,
      _cbCachePolicyId = Nothing,
      _cbPathPattern = pPathPattern_,
      _cbTargetOriginId = pTargetOriginId_,
      _cbViewerProtocolPolicy = pViewerProtocolPolicy_
    }

-- | Undocumented member.
cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\s a -> s {_cbAllowedMethods = a})

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
cbLambdaFunctionAssociations :: Lens' CacheBehavior (Maybe LambdaFunctionAssociations)
cbLambdaFunctionAssociations = lens _cbLambdaFunctionAssociations (\s a -> s {_cbLambdaFunctionAssociations = a})

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbMaxTTL :: Lens' CacheBehavior (Maybe Integer)
cbMaxTTL = lens _cbMaxTTL (\s a -> s {_cbMaxTTL = a})

-- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
cbMinTTL :: Lens' CacheBehavior (Maybe Integer)
cbMinTTL = lens _cbMinTTL (\s a -> s {_cbMinTTL = a})

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
cbCompress :: Lens' CacheBehavior (Maybe Bool)
cbCompress = lens _cbCompress (\s a -> s {_cbCompress = a})

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming = lens _cbSmoothStreaming (\s a -> s {_cbSmoothStreaming = a})

-- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies. When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
cbTrustedKeyGroups :: Lens' CacheBehavior (Maybe TrustedKeyGroups)
cbTrustedKeyGroups = lens _cbTrustedKeyGroups (\s a -> s {_cbTrustedKeyGroups = a})

-- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
cbRealtimeLogConfigARN :: Lens' CacheBehavior (Maybe Text)
cbRealtimeLogConfigARN = lens _cbRealtimeLogConfigARN (\s a -> s {_cbRealtimeLogConfigARN = a})

-- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbDefaultTTL :: Lens' CacheBehavior (Maybe Integer)
cbDefaultTTL = lens _cbDefaultTTL (\s a -> s {_cbDefaultTTL = a})

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ . If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ . If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ . A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
cbForwardedValues :: Lens' CacheBehavior (Maybe ForwardedValues)
cbForwardedValues = lens _cbForwardedValues (\s a -> s {_cbForwardedValues = a})

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ . A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies. When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
cbTrustedSigners :: Lens' CacheBehavior (Maybe TrustedSigners)
cbTrustedSigners = lens _cbTrustedSigners (\s a -> s {_cbTrustedSigners = a})

-- | The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
cbOriginRequestPolicyId :: Lens' CacheBehavior (Maybe Text)
cbOriginRequestPolicyId = lens _cbOriginRequestPolicyId (\s a -> s {_cbOriginRequestPolicyId = a})

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
cbFieldLevelEncryptionId :: Lens' CacheBehavior (Maybe Text)
cbFieldLevelEncryptionId = lens _cbFieldLevelEncryptionId (\s a -> s {_cbFieldLevelEncryptionId = a})

-- | The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
cbCachePolicyId :: Lens' CacheBehavior (Maybe Text)
cbCachePolicyId = lens _cbCachePolicyId (\s a -> s {_cbCachePolicyId = a})

-- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\s a -> s {_cbPathPattern = a})

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\s a -> s {_cbTargetOriginId = a})

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = lens _cbViewerProtocolPolicy (\s a -> s {_cbViewerProtocolPolicy = a})

instance FromXML CacheBehavior where
  parseXML x =
    CacheBehavior'
      <$> (x .@? "AllowedMethods")
      <*> (x .@? "LambdaFunctionAssociations")
      <*> (x .@? "MaxTTL")
      <*> (x .@? "MinTTL")
      <*> (x .@? "Compress")
      <*> (x .@? "SmoothStreaming")
      <*> (x .@? "TrustedKeyGroups")
      <*> (x .@? "RealtimeLogConfigArn")
      <*> (x .@? "DefaultTTL")
      <*> (x .@? "ForwardedValues")
      <*> (x .@? "TrustedSigners")
      <*> (x .@? "OriginRequestPolicyId")
      <*> (x .@? "FieldLevelEncryptionId")
      <*> (x .@? "CachePolicyId")
      <*> (x .@ "PathPattern")
      <*> (x .@ "TargetOriginId")
      <*> (x .@ "ViewerProtocolPolicy")

instance Hashable CacheBehavior

instance NFData CacheBehavior

instance ToXML CacheBehavior where
  toXML CacheBehavior' {..} =
    mconcat
      [ "AllowedMethods" @= _cbAllowedMethods,
        "LambdaFunctionAssociations" @= _cbLambdaFunctionAssociations,
        "MaxTTL" @= _cbMaxTTL,
        "MinTTL" @= _cbMinTTL,
        "Compress" @= _cbCompress,
        "SmoothStreaming" @= _cbSmoothStreaming,
        "TrustedKeyGroups" @= _cbTrustedKeyGroups,
        "RealtimeLogConfigArn" @= _cbRealtimeLogConfigARN,
        "DefaultTTL" @= _cbDefaultTTL,
        "ForwardedValues" @= _cbForwardedValues,
        "TrustedSigners" @= _cbTrustedSigners,
        "OriginRequestPolicyId" @= _cbOriginRequestPolicyId,
        "FieldLevelEncryptionId" @= _cbFieldLevelEncryptionId,
        "CachePolicyId" @= _cbCachePolicyId,
        "PathPattern" @= _cbPathPattern,
        "TargetOriginId" @= _cbTargetOriginId,
        "ViewerProtocolPolicy" @= _cbViewerProtocolPolicy
      ]
