{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DefaultCacheBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DefaultCacheBehavior
  ( DefaultCacheBehavior (..),

    -- * Smart constructor
    mkDefaultCacheBehavior,

    -- * Lenses
    dcbTargetOriginId,
    dcbViewerProtocolPolicy,
    dcbAllowedMethods,
    dcbCachePolicyId,
    dcbCompress,
    dcbDefaultTTL,
    dcbFieldLevelEncryptionId,
    dcbForwardedValues,
    dcbLambdaFunctionAssociations,
    dcbMaxTTL,
    dcbMinTTL,
    dcbOriginRequestPolicyId,
    dcbRealtimeLogConfigArn,
    dcbSmoothStreaming,
    dcbTrustedKeyGroups,
    dcbTrustedSigners,
  )
where

import qualified Network.AWS.CloudFront.Types.AllowedMethods as Types
import qualified Network.AWS.CloudFront.Types.ForwardedValues as Types
import qualified Network.AWS.CloudFront.Types.LambdaFunctionAssociations as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.CloudFront.Types.TrustedKeyGroups as Types
import qualified Network.AWS.CloudFront.Types.TrustedSigners as Types
import qualified Network.AWS.CloudFront.Types.ViewerProtocolPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that describes the default cache behavior if you don’t specify a @CacheBehavior@ element or if request URLs don’t match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- /See:/ 'mkDefaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
  { -- | The value of @ID@ for the origin that you want CloudFront to route requests to when they use the default cache behavior.
    targetOriginId :: Types.String,
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
    viewerProtocolPolicy :: Types.ViewerProtocolPolicy,
    allowedMethods :: Core.Maybe Types.AllowedMethods,
    -- | The unique identifier of the cache policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    cachePolicyId :: Core.Maybe Types.String,
    -- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
    compress :: Core.Maybe Core.Bool,
    -- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    defaultTTL :: Core.Maybe Core.Integer,
    -- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for the default cache behavior.
    fieldLevelEncryptionId :: Core.Maybe Types.String,
    -- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    -- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
    -- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
    forwardedValues :: Core.Maybe Types.ForwardedValues,
    -- | A complex type that contains zero or more Lambda function associations for a cache behavior.
    lambdaFunctionAssociations :: Core.Maybe Types.LambdaFunctionAssociations,
    -- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    maxTTL :: Core.Maybe Core.Integer,
    -- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
    --
    -- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
    -- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
    minTTL :: Core.Maybe Core.Integer,
    -- | The unique identifier of the origin request policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
    originRequestPolicyId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
    realtimeLogConfigArn :: Core.Maybe Types.String,
    -- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
    smoothStreaming :: Core.Maybe Core.Bool,
    -- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
    --
    -- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
    trustedKeyGroups :: Core.Maybe Types.TrustedKeyGroups,
    -- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
    --
    -- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
    -- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in a trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
    trustedSigners :: Core.Maybe Types.TrustedSigners
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultCacheBehavior' value with any optional fields omitted.
mkDefaultCacheBehavior ::
  -- | 'targetOriginId'
  Types.String ->
  -- | 'viewerProtocolPolicy'
  Types.ViewerProtocolPolicy ->
  DefaultCacheBehavior
mkDefaultCacheBehavior targetOriginId viewerProtocolPolicy =
  DefaultCacheBehavior'
    { targetOriginId,
      viewerProtocolPolicy,
      allowedMethods = Core.Nothing,
      cachePolicyId = Core.Nothing,
      compress = Core.Nothing,
      defaultTTL = Core.Nothing,
      fieldLevelEncryptionId = Core.Nothing,
      forwardedValues = Core.Nothing,
      lambdaFunctionAssociations = Core.Nothing,
      maxTTL = Core.Nothing,
      minTTL = Core.Nothing,
      originRequestPolicyId = Core.Nothing,
      realtimeLogConfigArn = Core.Nothing,
      smoothStreaming = Core.Nothing,
      trustedKeyGroups = Core.Nothing,
      trustedSigners = Core.Nothing
    }

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when they use the default cache behavior.
--
-- /Note:/ Consider using 'targetOriginId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTargetOriginId :: Lens.Lens' DefaultCacheBehavior Types.String
dcbTargetOriginId = Lens.field @"targetOriginId"
{-# DEPRECATED dcbTargetOriginId "Use generic-lens or generic-optics with 'targetOriginId' instead." #-}

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
{-# DEPRECATED dcbViewerProtocolPolicy "Use generic-lens or generic-optics with 'viewerProtocolPolicy' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbAllowedMethods :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.AllowedMethods)
dcbAllowedMethods = Lens.field @"allowedMethods"
{-# DEPRECATED dcbAllowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead." #-}

-- | The unique identifier of the cache policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'cachePolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbCachePolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.String)
dcbCachePolicyId = Lens.field @"cachePolicyId"
{-# DEPRECATED dcbCachePolicyId "Use generic-lens or generic-optics with 'cachePolicyId' instead." #-}

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'compress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbCompress :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
dcbCompress = Lens.field @"compress"
{-# DEPRECATED dcbCompress "Use generic-lens or generic-optics with 'compress' instead." #-}

-- | This field is deprecated. We recommend that you use the @DefaultTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbDefaultTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbDefaultTTL = Lens.field @"defaultTTL"
{-# DEPRECATED dcbDefaultTTL "Use generic-lens or generic-optics with 'defaultTTL' instead." #-}

-- | The value of @ID@ for the field-level encryption configuration that you want CloudFront to use for encrypting specific fields of data for the default cache behavior.
--
-- /Note:/ Consider using 'fieldLevelEncryptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbFieldLevelEncryptionId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.String)
dcbFieldLevelEncryptionId = Lens.field @"fieldLevelEncryptionId"
{-# DEPRECATED dcbFieldLevelEncryptionId "Use generic-lens or generic-optics with 'fieldLevelEncryptionId' instead." #-}

-- | This field is deprecated. We recommend that you use a cache policy or an origin request policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies> in the /Amazon CloudFront Developer Guide/ .
--
-- If you want to include values in the cache key, use a cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
-- If you want to send values to the origin but not include them in the cache key, use an origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
-- A complex type that specifies how CloudFront handles query strings, cookies, and HTTP headers.
--
-- /Note:/ Consider using 'forwardedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbForwardedValues :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.ForwardedValues)
dcbForwardedValues = Lens.field @"forwardedValues"
{-# DEPRECATED dcbForwardedValues "Use generic-lens or generic-optics with 'forwardedValues' instead." #-}

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- /Note:/ Consider using 'lambdaFunctionAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbLambdaFunctionAssociations :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.LambdaFunctionAssociations)
dcbLambdaFunctionAssociations = Lens.field @"lambdaFunctionAssociations"
{-# DEPRECATED dcbLambdaFunctionAssociations "Use generic-lens or generic-optics with 'lambdaFunctionAssociations' instead." #-}

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'maxTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbMaxTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbMaxTTL = Lens.field @"maxTTL"
{-# DEPRECATED dcbMaxTTL "Use generic-lens or generic-optics with 'maxTTL' instead." #-}

-- | This field is deprecated. We recommend that you use the @MinTTL@ field in a cache policy instead of this field. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies> in the /Amazon CloudFront Developer Guide/ .
--
-- The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
--
-- /Note:/ Consider using 'minTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbMinTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
dcbMinTTL = Lens.field @"minTTL"
{-# DEPRECATED dcbMinTTL "Use generic-lens or generic-optics with 'minTTL' instead." #-}

-- | The unique identifier of the origin request policy that is attached to the default cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies> or <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbOriginRequestPolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.String)
dcbOriginRequestPolicyId = Lens.field @"originRequestPolicyId"
{-# DEPRECATED dcbOriginRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead." #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration that is attached to this cache behavior. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'realtimeLogConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbRealtimeLogConfigArn :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.String)
dcbRealtimeLogConfigArn = Lens.field @"realtimeLogConfigArn"
{-# DEPRECATED dcbRealtimeLogConfigArn "Use generic-lens or generic-optics with 'realtimeLogConfigArn' instead." #-}

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- /Note:/ Consider using 'smoothStreaming' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbSmoothStreaming :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
dcbSmoothStreaming = Lens.field @"smoothStreaming"
{-# DEPRECATED dcbSmoothStreaming "Use generic-lens or generic-optics with 'smoothStreaming' instead." #-}

-- | A list of key groups that CloudFront can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with a private key whose corresponding public key is in the key group. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedKeyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTrustedKeyGroups :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.TrustedKeyGroups)
dcbTrustedKeyGroups = Lens.field @"trustedKeyGroups"
{-# DEPRECATED dcbTrustedKeyGroups "Use generic-lens or generic-optics with 'trustedKeyGroups' instead." #-}

-- | /Important:/ We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@ .
--
-- A list of AWS account IDs whose public keys CloudFront can use to validate signed URLs or signed cookies.
-- When a cache behavior contains trusted signers, CloudFront requires signed URLs or signed cookies for all requests that match the cache behavior. The URLs or cookies must be signed with the private key of a CloudFront key pair in a trusted signer’s AWS account. The signed URL or cookie contains information about which public key CloudFront should use to verify the signature. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcbTrustedSigners :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Types.TrustedSigners)
dcbTrustedSigners = Lens.field @"trustedSigners"
{-# DEPRECATED dcbTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

instance Core.ToXML DefaultCacheBehavior where
  toXML DefaultCacheBehavior {..} =
    Core.toXMLNode "TargetOriginId" targetOriginId
      Core.<> Core.toXMLNode "ViewerProtocolPolicy" viewerProtocolPolicy
      Core.<> Core.toXMLNode "AllowedMethods"
      Core.<$> allowedMethods
      Core.<> Core.toXMLNode "CachePolicyId"
      Core.<$> cachePolicyId
      Core.<> Core.toXMLNode "Compress"
      Core.<$> compress
      Core.<> Core.toXMLNode "DefaultTTL"
      Core.<$> defaultTTL
      Core.<> Core.toXMLNode "FieldLevelEncryptionId"
      Core.<$> fieldLevelEncryptionId
      Core.<> Core.toXMLNode "ForwardedValues"
      Core.<$> forwardedValues
      Core.<> Core.toXMLNode "LambdaFunctionAssociations"
      Core.<$> lambdaFunctionAssociations
      Core.<> Core.toXMLNode "MaxTTL"
      Core.<$> maxTTL
      Core.<> Core.toXMLNode "MinTTL"
      Core.<$> minTTL
      Core.<> Core.toXMLNode "OriginRequestPolicyId"
      Core.<$> originRequestPolicyId
      Core.<> Core.toXMLNode "RealtimeLogConfigArn"
      Core.<$> realtimeLogConfigArn
      Core.<> Core.toXMLNode "SmoothStreaming"
      Core.<$> smoothStreaming
      Core.<> Core.toXMLNode "TrustedKeyGroups"
      Core.<$> trustedKeyGroups
      Core.<> Core.toXMLNode "TrustedSigners"
      Core.<$> trustedSigners

instance Core.FromXML DefaultCacheBehavior where
  parseXML x =
    DefaultCacheBehavior'
      Core.<$> (x Core..@ "TargetOriginId")
      Core.<*> (x Core..@ "ViewerProtocolPolicy")
      Core.<*> (x Core..@? "AllowedMethods")
      Core.<*> (x Core..@? "CachePolicyId")
      Core.<*> (x Core..@? "Compress")
      Core.<*> (x Core..@? "DefaultTTL")
      Core.<*> (x Core..@? "FieldLevelEncryptionId")
      Core.<*> (x Core..@? "ForwardedValues")
      Core.<*> (x Core..@? "LambdaFunctionAssociations")
      Core.<*> (x Core..@? "MaxTTL")
      Core.<*> (x Core..@? "MinTTL")
      Core.<*> (x Core..@? "OriginRequestPolicyId")
      Core.<*> (x Core..@? "RealtimeLogConfigArn")
      Core.<*> (x Core..@? "SmoothStreaming")
      Core.<*> (x Core..@? "TrustedKeyGroups")
      Core.<*> (x Core..@? "TrustedSigners")
