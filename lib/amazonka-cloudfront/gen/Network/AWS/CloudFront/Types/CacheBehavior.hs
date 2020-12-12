{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehavior
  ( CacheBehavior (..),

    -- * Smart constructor
    mkCacheBehavior,

    -- * Lenses
    cbAllowedMethods,
    cbLambdaFunctionAssociations,
    cbMaxTTL,
    cbMinTTL,
    cbCompress,
    cbSmoothStreaming,
    cbTrustedKeyGroups,
    cbRealtimeLogConfigARN,
    cbDefaultTTL,
    cbForwardedValues,
    cbTrustedSigners,
    cbOriginRequestPolicyId,
    cbFieldLevelEncryptionId,
    cbCachePolicyId,
    cbPathPattern,
    cbTargetOriginId,
    cbViewerProtocolPolicy,
  )
where

import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.TrustedKeyGroups
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that describes how CloudFront processes requests.
--
-- You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to serve objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.
-- For the current quota (formerly known as limit) on the number of cache behaviors that you can add to a distribution, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> in the /Amazon CloudFront Developer Guide/ .
-- If you don’t want to specify any cache behaviors, include only an empty @CacheBehaviors@ element. Don’t include an empty @CacheBehavior@ element because this is invalid.
-- To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty @CacheBehaviors@ element.
-- To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.
-- For more information about cache behaviors, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { allowedMethods ::
      Lude.Maybe AllowedMethods,
    lambdaFunctionAssociations ::
      Lude.Maybe LambdaFunctionAssociations,
    maxTTL :: Lude.Maybe Lude.Integer,
    minTTL :: Lude.Maybe Lude.Integer,
    compress :: Lude.Maybe Lude.Bool,
    smoothStreaming :: Lude.Maybe Lude.Bool,
    trustedKeyGroups :: Lude.Maybe TrustedKeyGroups,
    realtimeLogConfigARN :: Lude.Maybe Lude.Text,
    defaultTTL :: Lude.Maybe Lude.Integer,
    forwardedValues :: Lude.Maybe ForwardedValues,
    trustedSigners :: Lude.Maybe TrustedSigners,
    originRequestPolicyId :: Lude.Maybe Lude.Text,
    fieldLevelEncryptionId :: Lude.Maybe Lude.Text,
    cachePolicyId :: Lude.Maybe Lude.Text,
    pathPattern :: Lude.Text,
    targetOriginId :: Lude.Text,
    viewerProtocolPolicy :: ViewerProtocolPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- * 'allowedMethods' - Undocumented field.
-- * 'cachePolicyId' - The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- * 'compress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
-- * 'defaultTTL' - This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- * 'fieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
-- * 'forwardedValues' - This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
-- * 'lambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
-- * 'maxTTL' - This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- * 'minTTL' - This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
-- * 'originRequestPolicyId' - The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- * 'pathPattern' - The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution.
--
-- The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
-- * 'realtimeLogConfigARN' - The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
-- * 'smoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
-- * 'targetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
-- * 'trustedKeyGroups' - A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
-- * 'trustedSigners' - /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
-- * 'viewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:
--
--
--     * @allow-all@ : Viewers can use HTTP or HTTPS.
--
--
--     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.
--
--
--     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).
--
--
-- For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
mkCacheBehavior ::
  -- | 'pathPattern'
  Lude.Text ->
  -- | 'targetOriginId'
  Lude.Text ->
  -- | 'viewerProtocolPolicy'
  ViewerProtocolPolicy ->
  CacheBehavior
mkCacheBehavior
  pPathPattern_
  pTargetOriginId_
  pViewerProtocolPolicy_ =
    CacheBehavior'
      { allowedMethods = Lude.Nothing,
        lambdaFunctionAssociations = Lude.Nothing,
        maxTTL = Lude.Nothing,
        minTTL = Lude.Nothing,
        compress = Lude.Nothing,
        smoothStreaming = Lude.Nothing,
        trustedKeyGroups = Lude.Nothing,
        realtimeLogConfigARN = Lude.Nothing,
        defaultTTL = Lude.Nothing,
        forwardedValues = Lude.Nothing,
        trustedSigners = Lude.Nothing,
        originRequestPolicyId = Lude.Nothing,
        fieldLevelEncryptionId = Lude.Nothing,
        cachePolicyId = Lude.Nothing,
        pathPattern = pPathPattern_,
        targetOriginId = pTargetOriginId_,
        viewerProtocolPolicy = pViewerProtocolPolicy_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbAllowedMethods :: Lens.Lens' CacheBehavior (Lude.Maybe AllowedMethods)
cbAllowedMethods = Lens.lens (allowedMethods :: CacheBehavior -> Lude.Maybe AllowedMethods) (\s a -> s {allowedMethods = a} :: CacheBehavior)
{-# DEPRECATED cbAllowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead." #-}

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- /Note:/ Consider using 'lambdaFunctionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbLambdaFunctionAssociations :: Lens.Lens' CacheBehavior (Lude.Maybe LambdaFunctionAssociations)
cbLambdaFunctionAssociations = Lens.lens (lambdaFunctionAssociations :: CacheBehavior -> Lude.Maybe LambdaFunctionAssociations) (\s a -> s {lambdaFunctionAssociations = a} :: CacheBehavior)
{-# DEPRECATED cbLambdaFunctionAssociations "Use generic-lens or generic-optics with 'lambdaFunctionAssociations' instead." #-}

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'maxTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMaxTTL :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Integer)
cbMaxTTL = Lens.lens (maxTTL :: CacheBehavior -> Lude.Maybe Lude.Integer) (\s a -> s {maxTTL = a} :: CacheBehavior)
{-# DEPRECATED cbMaxTTL "Use generic-lens or generic-optics with 'maxTTL' instead." #-}

-- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
--
-- /Note:/ Consider using 'minTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbMinTTL :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Integer)
cbMinTTL = Lens.lens (minTTL :: CacheBehavior -> Lude.Maybe Lude.Integer) (\s a -> s {minTTL = a} :: CacheBehavior)
{-# DEPRECATED cbMinTTL "Use generic-lens or generic-optics with 'minTTL' instead." #-}

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'compress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCompress :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Bool)
cbCompress = Lens.lens (compress :: CacheBehavior -> Lude.Maybe Lude.Bool) (\s a -> s {compress = a} :: CacheBehavior)
{-# DEPRECATED cbCompress "Use generic-lens or generic-optics with 'compress' instead." #-}

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- /Note:/ Consider using 'smoothStreaming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbSmoothStreaming :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Bool)
cbSmoothStreaming = Lens.lens (smoothStreaming :: CacheBehavior -> Lude.Maybe Lude.Bool) (\s a -> s {smoothStreaming = a} :: CacheBehavior)
{-# DEPRECATED cbSmoothStreaming "Use generic-lens or generic-optics with 'smoothStreaming' instead." #-}

-- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTrustedKeyGroups :: Lens.Lens' CacheBehavior (Lude.Maybe TrustedKeyGroups)
cbTrustedKeyGroups = Lens.lens (trustedKeyGroups :: CacheBehavior -> Lude.Maybe TrustedKeyGroups) (\s a -> s {trustedKeyGroups = a} :: CacheBehavior)
{-# DEPRECATED cbTrustedKeyGroups "Use generic-lens or generic-optics with 'trustedKeyGroups' instead." #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'realtimeLogConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbRealtimeLogConfigARN :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Text)
cbRealtimeLogConfigARN = Lens.lens (realtimeLogConfigARN :: CacheBehavior -> Lude.Maybe Lude.Text) (\s a -> s {realtimeLogConfigARN = a} :: CacheBehavior)
{-# DEPRECATED cbRealtimeLogConfigARN "Use generic-lens or generic-optics with 'realtimeLogConfigARN' instead." #-}

-- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbDefaultTTL :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Integer)
cbDefaultTTL = Lens.lens (defaultTTL :: CacheBehavior -> Lude.Maybe Lude.Integer) (\s a -> s {defaultTTL = a} :: CacheBehavior)
{-# DEPRECATED cbDefaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- /Note:/ Consider using 'forwardedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbForwardedValues :: Lens.Lens' CacheBehavior (Lude.Maybe ForwardedValues)
cbForwardedValues = Lens.lens (forwardedValues :: CacheBehavior -> Lude.Maybe ForwardedValues) (\s a -> s {forwardedValues = a} :: CacheBehavior)
{-# DEPRECATED cbForwardedValues "Use generic-lens or generic-optics with 'forwardedValues' instead." #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in the trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTrustedSigners :: Lens.Lens' CacheBehavior (Lude.Maybe TrustedSigners)
cbTrustedSigners = Lens.lens (trustedSigners :: CacheBehavior -> Lude.Maybe TrustedSigners) (\s a -> s {trustedSigners = a} :: CacheBehavior)
{-# DEPRECATED cbTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

-- | The unique identifier of the origin request policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbOriginRequestPolicyId :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Text)
cbOriginRequestPolicyId = Lens.lens (originRequestPolicyId :: CacheBehavior -> Lude.Maybe Lude.Text) (\s a -> s {originRequestPolicyId = a} :: CacheBehavior)
{-# DEPRECATED cbOriginRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead." #-}

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for this cache behavior.
--
-- /Note:/ Consider using 'fieldLevelEncryptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbFieldLevelEncryptionId :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Text)
cbFieldLevelEncryptionId = Lens.lens (fieldLevelEncryptionId :: CacheBehavior -> Lude.Maybe Lude.Text) (\s a -> s {fieldLevelEncryptionId = a} :: CacheBehavior)
{-# DEPRECATED cbFieldLevelEncryptionId "Use generic-lens or generic-optics with 'fieldLevelEncryptionId' instead." #-}

-- | The unique identifier of the cache policy that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCachePolicyId :: Lens.Lens' CacheBehavior (Lude.Maybe Lude.Text)
cbCachePolicyId = Lens.lens (cachePolicyId :: CacheBehavior -> Lude.Maybe Lude.Text) (\s a -> s {cachePolicyId = a} :: CacheBehavior)
{-# DEPRECATED cbCachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead." #-}

-- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution.
--
-- The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'pathPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbPathPattern :: Lens.Lens' CacheBehavior Lude.Text
cbPathPattern = Lens.lens (pathPattern :: CacheBehavior -> Lude.Text) (\s a -> s {pathPattern = a} :: CacheBehavior)
{-# DEPRECATED cbPathPattern "Use generic-lens or generic-optics with 'pathPattern' instead." #-}

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when they match this cache behavior.
--
-- /Note:/ Consider using 'targetOriginId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTargetOriginId :: Lens.Lens' CacheBehavior Lude.Text
cbTargetOriginId = Lens.lens (targetOriginId :: CacheBehavior -> Lude.Text) (\s a -> s {targetOriginId = a} :: CacheBehavior)
{-# DEPRECATED cbTargetOriginId "Use generic-lens or generic-optics with 'targetOriginId' instead." #-}

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:
--
--
--     * @allow-all@ : Viewers can use HTTP or HTTPS.
--
--
--     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.
--
--
--     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).
--
--
-- For more information about requiring the HTTPS protocol, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'viewerProtocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbViewerProtocolPolicy :: Lens.Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = Lens.lens (viewerProtocolPolicy :: CacheBehavior -> ViewerProtocolPolicy) (\s a -> s {viewerProtocolPolicy = a} :: CacheBehavior)
{-# DEPRECATED cbViewerProtocolPolicy "Use generic-lens or generic-optics with 'viewerProtocolPolicy' instead." #-}

instance Lude.FromXML CacheBehavior where
  parseXML x =
    CacheBehavior'
      Lude.<$> (x Lude..@? "AllowedMethods")
      Lude.<*> (x Lude..@? "LambdaFunctionAssociations")
      Lude.<*> (x Lude..@? "MaxTTL")
      Lude.<*> (x Lude..@? "MinTTL")
      Lude.<*> (x Lude..@? "Compress")
      Lude.<*> (x Lude..@? "SmoothStreaming")
      Lude.<*> (x Lude..@? "TrustedKeyGroups")
      Lude.<*> (x Lude..@? "RealtimeLogConfigArn")
      Lude.<*> (x Lude..@? "DefaultTTL")
      Lude.<*> (x Lude..@? "ForwardedValues")
      Lude.<*> (x Lude..@? "TrustedSigners")
      Lude.<*> (x Lude..@? "OriginRequestPolicyId")
      Lude.<*> (x Lude..@? "FieldLevelEncryptionId")
      Lude.<*> (x Lude..@? "CachePolicyId")
      Lude.<*> (x Lude..@ "PathPattern")
      Lude.<*> (x Lude..@ "TargetOriginId")
      Lude.<*> (x Lude..@ "ViewerProtocolPolicy")

instance Lude.ToXML CacheBehavior where
  toXML CacheBehavior' {..} =
    Lude.mconcat
      [ "AllowedMethods" Lude.@= allowedMethods,
        "LambdaFunctionAssociations" Lude.@= lambdaFunctionAssociations,
        "MaxTTL" Lude.@= maxTTL,
        "MinTTL" Lude.@= minTTL,
        "Compress" Lude.@= compress,
        "SmoothStreaming" Lude.@= smoothStreaming,
        "TrustedKeyGroups" Lude.@= trustedKeyGroups,
        "RealtimeLogConfigArn" Lude.@= realtimeLogConfigARN,
        "DefaultTTL" Lude.@= defaultTTL,
        "ForwardedValues" Lude.@= forwardedValues,
        "TrustedSigners" Lude.@= trustedSigners,
        "OriginRequestPolicyId" Lude.@= originRequestPolicyId,
        "FieldLevelEncryptionId" Lude.@= fieldLevelEncryptionId,
        "CachePolicyId" Lude.@= cachePolicyId,
        "PathPattern" Lude.@= pathPattern,
        "TargetOriginId" Lude.@= targetOriginId,
        "ViewerProtocolPolicy" Lude.@= viewerProtocolPolicy
      ]
