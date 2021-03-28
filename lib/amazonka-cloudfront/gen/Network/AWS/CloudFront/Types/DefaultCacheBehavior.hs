{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DefaultCacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.DefaultCacheBehavior
  ( DefaultCacheBehavior (..)
  -- * Smart constructor
  , mkDefaultCacheBehavior
  -- * Lenses
  , dcbTargetOriginId
  , dcbViewerProtocolPolicy
  , dcbAllowedMethods
  , dcbCachePolicyId
  , dcbCompress
  , dcbDefaultTTL
  , dcbFieldLevelEncryptionId
  , dcbForwardedValues
  , dcbLambdaFunctionAssociations
  , dcbMaxTTL
  , dcbMinTTL
  , dcbOriginRequestPolicyId
  , dcbRealtimeLogConfigArn
  , dcbSmoothStreaming
  , dcbTrustedKeyGroups
  , dcbTrustedSigners
  ) where

import qualified Network.AWS.CloudFront.Types.AllowedMethods as Types
import qualified Network.AWS.CloudFront.Types.ForwardedValues as Types
import qualified Network.AWS.CloudFront.Types.LambdaFunctionAssociations as Types
import qualified Network.AWS.CloudFront.Types.TrustedKeyGroups as Types
import qualified Network.AWS.CloudFront.Types.TrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.ViewerProtocolPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that describes the default cache behavior if you don’t specify a @CacheBehavior@ element or if request URLs don’t match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /See:/ 'mkDefaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
  { targetOriginId :: Core.Text
    -- ^ The value of @ID@ for the origin that you want CloudFront to route requests to when they use the default cache behavior.
  , viewerProtocolPolicy :: Types.ViewerProtocolPolicy
    -- ^ The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:
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
  , allowedMethods :: Core.Maybe Types.AllowedMethods
  , cachePolicyId :: Core.Maybe Core.Text
    -- ^ The unique identifier of the cache policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
  , compress :: Core.Maybe Core.Bool
    -- ^ Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
  , defaultTTL :: Core.Maybe Core.Integer
    -- ^ This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
  , fieldLevelEncryptionId :: Core.Maybe Core.Text
    -- ^ The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for the default cache behavior.
  , forwardedValues :: Core.Maybe Types.ForwardedValues
    -- ^ This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
  , lambdaFunctionAssociations :: Core.Maybe Types.LambdaFunctionAssociations
    -- ^ A complex type that contains zero or more Lambda function associations for a cache behavior.
  , maxTTL :: Core.Maybe Core.Integer
    -- ^ This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
  , minTTL :: Core.Maybe Core.Integer
    -- ^ This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
  , originRequestPolicyId :: Core.Maybe Core.Text
    -- ^ The unique identifier of the origin request policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
  , realtimeLogConfigArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
  , smoothStreaming :: Core.Maybe Core.Bool
    -- ^ Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
  , trustedKeyGroups :: Core.Maybe Types.TrustedKeyGroups
    -- ^ A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
  , trustedSigners :: Core.Maybe Types.TrustedSigners
    -- ^ /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in a trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultCacheBehavior' value with any optional fields omitted.
mkDefaultCacheBehavior
    :: Core.Text -- ^ 'targetOriginId'
    -> Types.ViewerProtocolPolicy -- ^ 'viewerProtocolPolicy'
    -> DefaultCacheBehavior
mkDefaultCacheBehavior targetOriginId viewerProtocolPolicy
  = DefaultCacheBehavior'{targetOriginId, viewerProtocolPolicy,
                          allowedMethods = Core.Nothing, cachePolicyId = Core.Nothing,
                          compress = Core.Nothing, defaultTTL = Core.Nothing,
                          fieldLevelEncryptionId = Core.Nothing,
                          forwardedValues = Core.Nothing,
                          lambdaFunctionAssociations = Core.Nothing, maxTTL = Core.Nothing,
                          minTTL = Core.Nothing, originRequestPolicyId = Core.Nothing,
                          realtimeLogConfigArn = Core.Nothing,
                          smoothStreaming = Core.Nothing, trustedKeyGroups = Core.Nothing,
                          trustedSigners = Core.Nothing}

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when they use the default cache behavior.
--
-- /Note:/ Consider using 'targetOriginId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTargetOriginId :: Lens.Lens' DefaultCacheBehavior Core.Text
dcbTargetOriginId = Lens.field @"targetOriginId"
{-# INLINEABLE dcbTargetOriginId #-}
{-# DEPRECATED targetOriginId "Use generic-lens or generic-optics with 'targetOriginId' instead"  #-}

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
dcbViewerProtocolPolicy :: Lens.Lens' DefaultCacheBehavior Types.ViewerProtocolPolicy
dcbViewerProtocolPolicy = Lens.field @"viewerProtocolPolicy"
{-# INLINEABLE dcbViewerProtocolPolicy #-}
{-# DEPRECATED viewerProtocolPolicy "Use generic-lens or generic-optics with 'viewerProtocolPolicy' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbAllowedMethods :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.AllowedMethods)
dcbAllowedMethods = Lens.field @"allowedMethods"
{-# INLINEABLE dcbAllowedMethods #-}
{-# DEPRECATED allowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead"  #-}

-- | The unique identifier of the cache policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbCachePolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
dcbCachePolicyId = Lens.field @"cachePolicyId"
{-# INLINEABLE dcbCachePolicyId #-}
{-# DEPRECATED cachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead"  #-}

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'compress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbCompress :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
dcbCompress = Lens.field @"compress"
{-# INLINEABLE dcbCompress #-}
{-# DEPRECATED compress "Use generic-lens or generic-optics with 'compress' instead"  #-}

-- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbDefaultTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbDefaultTTL = Lens.field @"defaultTTL"
{-# INLINEABLE dcbDefaultTTL #-}
{-# DEPRECATED defaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead"  #-}

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for the default cache behavior.
--
-- /Note:/ Consider using 'fieldLevelEncryptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbFieldLevelEncryptionId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
dcbFieldLevelEncryptionId = Lens.field @"fieldLevelEncryptionId"
{-# INLINEABLE dcbFieldLevelEncryptionId #-}
{-# DEPRECATED fieldLevelEncryptionId "Use generic-lens or generic-optics with 'fieldLevelEncryptionId' instead"  #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- /Note:/ Consider using 'forwardedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbForwardedValues :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.ForwardedValues)
dcbForwardedValues = Lens.field @"forwardedValues"
{-# INLINEABLE dcbForwardedValues #-}
{-# DEPRECATED forwardedValues "Use generic-lens or generic-optics with 'forwardedValues' instead"  #-}

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- /Note:/ Consider using 'lambdaFunctionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbLambdaFunctionAssociations :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.LambdaFunctionAssociations)
dcbLambdaFunctionAssociations = Lens.field @"lambdaFunctionAssociations"
{-# INLINEABLE dcbLambdaFunctionAssociations #-}
{-# DEPRECATED lambdaFunctionAssociations "Use generic-lens or generic-optics with 'lambdaFunctionAssociations' instead"  #-}

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'maxTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbMaxTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbMaxTTL = Lens.field @"maxTTL"
{-# INLINEABLE dcbMaxTTL #-}
{-# DEPRECATED maxTTL "Use generic-lens or generic-optics with 'maxTTL' instead"  #-}

-- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
--
-- /Note:/ Consider using 'minTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbMinTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbMinTTL = Lens.field @"minTTL"
{-# INLINEABLE dcbMinTTL #-}
{-# DEPRECATED minTTL "Use generic-lens or generic-optics with 'minTTL' instead"  #-}

-- | The unique identifier of the origin request policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbOriginRequestPolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
dcbOriginRequestPolicyId = Lens.field @"originRequestPolicyId"
{-# INLINEABLE dcbOriginRequestPolicyId #-}
{-# DEPRECATED originRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'realtimeLogConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbRealtimeLogConfigArn :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
dcbRealtimeLogConfigArn = Lens.field @"realtimeLogConfigArn"
{-# INLINEABLE dcbRealtimeLogConfigArn #-}
{-# DEPRECATED realtimeLogConfigArn "Use generic-lens or generic-optics with 'realtimeLogConfigArn' instead"  #-}

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ . 
--
-- /Note:/ Consider using 'smoothStreaming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbSmoothStreaming :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
dcbSmoothStreaming = Lens.field @"smoothStreaming"
{-# INLINEABLE dcbSmoothStreaming #-}
{-# DEPRECATED smoothStreaming "Use generic-lens or generic-optics with 'smoothStreaming' instead"  #-}

-- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTrustedKeyGroups :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.TrustedKeyGroups)
dcbTrustedKeyGroups = Lens.field @"trustedKeyGroups"
{-# INLINEABLE dcbTrustedKeyGroups #-}
{-# DEPRECATED trustedKeyGroups "Use generic-lens or generic-optics with 'trustedKeyGroups' instead"  #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in a trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTrustedSigners :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.TrustedSigners)
dcbTrustedSigners = Lens.field @"trustedSigners"
{-# INLINEABLE dcbTrustedSigners #-}
{-# DEPRECATED trustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead"  #-}

instance Core.ToXML DefaultCacheBehavior where
        toXML DefaultCacheBehavior{..}
          = Core.toXMLElement "TargetOriginId" targetOriginId Core.<>
              Core.toXMLElement "ViewerProtocolPolicy" viewerProtocolPolicy
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "AllowedMethods")
                allowedMethods
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "CachePolicyId")
                cachePolicyId
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Compress") compress
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "DefaultTTL") defaultTTL
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "FieldLevelEncryptionId")
                fieldLevelEncryptionId
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ForwardedValues")
                forwardedValues
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "LambdaFunctionAssociations")
                lambdaFunctionAssociations
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "MaxTTL") maxTTL
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "MinTTL") minTTL
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "OriginRequestPolicyId")
                originRequestPolicyId
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RealtimeLogConfigArn")
                realtimeLogConfigArn
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "SmoothStreaming")
                smoothStreaming
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "TrustedKeyGroups")
                trustedKeyGroups
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "TrustedSigners")
                trustedSigners

instance Core.FromXML DefaultCacheBehavior where
        parseXML x
          = DefaultCacheBehavior' Core.<$>
              (x Core..@ "TargetOriginId") Core.<*>
                x Core..@ "ViewerProtocolPolicy"
                Core.<*> x Core..@? "AllowedMethods"
                Core.<*> x Core..@? "CachePolicyId"
                Core.<*> x Core..@? "Compress"
                Core.<*> x Core..@? "DefaultTTL"
                Core.<*> x Core..@? "FieldLevelEncryptionId"
                Core.<*> x Core..@? "ForwardedValues"
                Core.<*> x Core..@? "LambdaFunctionAssociations"
                Core.<*> x Core..@? "MaxTTL"
                Core.<*> x Core..@? "MinTTL"
                Core.<*> x Core..@? "OriginRequestPolicyId"
                Core.<*> x Core..@? "RealtimeLogConfigArn"
                Core.<*> x Core..@? "SmoothStreaming"
                Core.<*> x Core..@? "TrustedKeyGroups"
                Core.<*> x Core..@? "TrustedSigners"
