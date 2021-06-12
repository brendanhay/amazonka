{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DefaultCacheBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DefaultCacheBehavior where

import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.TrustedKeyGroups
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that describes the default cache behavior if you don’t
-- specify a @CacheBehavior@ element or if request URLs don’t match any of
-- the values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
--
-- /See:/ 'newDefaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
  { -- | A complex type that contains zero or more Lambda function associations
    -- for a cache behavior.
    lambdaFunctionAssociations :: Core.Maybe LambdaFunctionAssociations,
    allowedMethods :: Core.Maybe AllowedMethods,
    -- | The unique identifier of the cache policy that is attached to the
    -- default cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    cachePolicyId :: Core.Maybe Core.Text,
    -- | Indicates whether you want to distribute media files in the Microsoft
    -- Smooth Streaming format using the origin that is associated with this
    -- cache behavior. If so, specify @true@; if not, specify @false@. If you
    -- specify @true@ for @SmoothStreaming@, you can still distribute other
    -- content using this cache behavior if the content matches the value of
    -- @PathPattern@.
    smoothStreaming :: Core.Maybe Core.Bool,
    -- | The value of @ID@ for the field-level encryption configuration that you
    -- want CloudFront to use for encrypting specific fields of data for the
    -- default cache behavior.
    fieldLevelEncryptionId :: Core.Maybe Core.Text,
    -- | The unique identifier of the origin request policy that is attached to
    -- the default cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    originRequestPolicyId :: Core.Maybe Core.Text,
    -- | This field is deprecated. We recommend that you use the @MaxTTL@ field
    -- in a cache policy instead of this field. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The maximum amount of time that you want objects to stay in CloudFront
    -- caches before CloudFront forwards another request to your origin to
    -- determine whether the object has been updated. The value that you
    -- specify applies only when your origin adds HTTP headers such as
    -- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
    -- objects. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    maxTTL :: Core.Maybe Core.Integer,
    -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to include values in the cache key, use a cache policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send values to the origin but not include them in the
    -- cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A complex type that specifies how CloudFront handles query strings,
    -- cookies, and HTTP headers.
    forwardedValues :: Core.Maybe ForwardedValues,
    -- | This field is deprecated. We recommend that you use the @DefaultTTL@
    -- field in a cache policy instead of this field. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The default amount of time that you want objects to stay in CloudFront
    -- caches before CloudFront forwards another request to your origin to
    -- determine whether the object has been updated. The value that you
    -- specify applies only when your origin does not add HTTP headers such as
    -- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
    -- objects. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    defaultTTL :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration that
    -- is attached to this cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
    -- in the /Amazon CloudFront Developer Guide/.
    realtimeLogConfigArn :: Core.Maybe Core.Text,
    -- | This field is deprecated. We recommend that you use the @MinTTL@ field
    -- in a cache policy instead of this field. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The minimum amount of time that you want objects to stay in CloudFront
    -- caches before CloudFront forwards another request to your origin to
    -- determine whether the object has been updated. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward
    -- all headers to your origin (under @Headers@, if you specify @1@ for
    -- @Quantity@ and @*@ for @Name@).
    minTTL :: Core.Maybe Core.Integer,
    -- | Whether you want CloudFront to automatically compress certain files for
    -- this cache behavior. If so, specify @true@; if not, specify @false@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
    -- in the /Amazon CloudFront Developer Guide/.
    compress :: Core.Maybe Core.Bool,
    -- | A list of key groups that CloudFront can use to validate signed URLs or
    -- signed cookies.
    --
    -- When a cache behavior contains trusted key groups, CloudFront requires
    -- signed URLs or signed cookies for all requests that match the cache
    -- behavior. The URLs or cookies must be signed with a private key whose
    -- corresponding public key is in the key group. The signed URL or cookie
    -- contains information about which public key CloudFront should use to
    -- verify the signature. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedKeyGroups :: Core.Maybe TrustedKeyGroups,
    -- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
    --
    -- A list of AWS account IDs whose public keys CloudFront can use to
    -- validate signed URLs or signed cookies.
    --
    -- When a cache behavior contains trusted signers, CloudFront requires
    -- signed URLs or signed cookies for all requests that match the cache
    -- behavior. The URLs or cookies must be signed with the private key of a
    -- CloudFront key pair in a trusted signer’s AWS account. The signed URL or
    -- cookie contains information about which public key CloudFront should use
    -- to verify the signature. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedSigners :: Core.Maybe TrustedSigners,
    -- | The value of @ID@ for the origin that you want CloudFront to route
    -- requests to when they use the default cache behavior.
    targetOriginId :: Core.Text,
    -- | The protocol that viewers can use to access the files in the origin
    -- specified by @TargetOriginId@ when a request matches the path pattern in
    -- @PathPattern@. You can specify the following options:
    --
    -- -   @allow-all@: Viewers can use HTTP or HTTPS.
    --
    -- -   @redirect-to-https@: If a viewer submits an HTTP request, CloudFront
    --     returns an HTTP status code of 301 (Moved Permanently) to the viewer
    --     along with the HTTPS URL. The viewer then resubmits the request
    --     using the new URL.
    --
    -- -   @https-only@: If a viewer sends an HTTP request, CloudFront returns
    --     an HTTP status code of 403 (Forbidden).
    --
    -- For more information about requiring the HTTPS protocol, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The only way to guarantee that viewers retrieve an object that was
    -- fetched from the origin using HTTPS is never to use any other protocol
    -- to fetch the object. If you have recently changed from HTTP to HTTPS, we
    -- recommend that you clear your objects’ cache because cached objects are
    -- protocol agnostic. That means that an edge location will return an
    -- object from the cache regardless of whether the current request protocol
    -- matches the protocol used previously. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
    -- in the /Amazon CloudFront Developer Guide/.
    viewerProtocolPolicy :: ViewerProtocolPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefaultCacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionAssociations', 'defaultCacheBehavior_lambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations
-- for a cache behavior.
--
-- 'allowedMethods', 'defaultCacheBehavior_allowedMethods' - Undocumented member.
--
-- 'cachePolicyId', 'defaultCacheBehavior_cachePolicyId' - The unique identifier of the cache policy that is attached to the
-- default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'smoothStreaming', 'defaultCacheBehavior_smoothStreaming' - Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
--
-- 'fieldLevelEncryptionId', 'defaultCacheBehavior_fieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for the
-- default cache behavior.
--
-- 'originRequestPolicyId', 'defaultCacheBehavior_originRequestPolicyId' - The unique identifier of the origin request policy that is attached to
-- the default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'maxTTL', 'defaultCacheBehavior_maxTTL' - This field is deprecated. We recommend that you use the @MaxTTL@ field
-- in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The maximum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. The value that you
-- specify applies only when your origin adds HTTP headers such as
-- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'forwardedValues', 'defaultCacheBehavior_forwardedValues' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to include values in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send values to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
--
-- 'defaultTTL', 'defaultCacheBehavior_defaultTTL' - This field is deprecated. We recommend that you use the @DefaultTTL@
-- field in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. The value that you
-- specify applies only when your origin does not add HTTP headers such as
-- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'realtimeLogConfigArn', 'defaultCacheBehavior_realtimeLogConfigArn' - The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'minTTL', 'defaultCacheBehavior_minTTL' - This field is deprecated. We recommend that you use the @MinTTL@ field
-- in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward
-- all headers to your origin (under @Headers@, if you specify @1@ for
-- @Quantity@ and @*@ for @Name@).
--
-- 'compress', 'defaultCacheBehavior_compress' - Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify @true@; if not, specify @false@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'trustedKeyGroups', 'defaultCacheBehavior_trustedKeyGroups' - A list of key groups that CloudFront can use to validate signed URLs or
-- signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with a private key whose
-- corresponding public key is in the key group. The signed URL or cookie
-- contains information about which public key CloudFront should use to
-- verify the signature. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'trustedSigners', 'defaultCacheBehavior_trustedSigners' - We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of AWS account IDs whose public keys CloudFront can use to
-- validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in a trusted signer’s AWS account. The signed URL or
-- cookie contains information about which public key CloudFront should use
-- to verify the signature. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'targetOriginId', 'defaultCacheBehavior_targetOriginId' - The value of @ID@ for the origin that you want CloudFront to route
-- requests to when they use the default cache behavior.
--
-- 'viewerProtocolPolicy', 'defaultCacheBehavior_viewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin
-- specified by @TargetOriginId@ when a request matches the path pattern in
-- @PathPattern@. You can specify the following options:
--
-- -   @allow-all@: Viewers can use HTTP or HTTPS.
--
-- -   @redirect-to-https@: If a viewer submits an HTTP request, CloudFront
--     returns an HTTP status code of 301 (Moved Permanently) to the viewer
--     along with the HTTPS URL. The viewer then resubmits the request
--     using the new URL.
--
-- -   @https-only@: If a viewer sends an HTTP request, CloudFront returns
--     an HTTP status code of 403 (Forbidden).
--
-- For more information about requiring the HTTPS protocol, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The only way to guarantee that viewers retrieve an object that was
-- fetched from the origin using HTTPS is never to use any other protocol
-- to fetch the object. If you have recently changed from HTTP to HTTPS, we
-- recommend that you clear your objects’ cache because cached objects are
-- protocol agnostic. That means that an edge location will return an
-- object from the cache regardless of whether the current request protocol
-- matches the protocol used previously. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
-- in the /Amazon CloudFront Developer Guide/.
newDefaultCacheBehavior ::
  -- | 'targetOriginId'
  Core.Text ->
  -- | 'viewerProtocolPolicy'
  ViewerProtocolPolicy ->
  DefaultCacheBehavior
newDefaultCacheBehavior
  pTargetOriginId_
  pViewerProtocolPolicy_ =
    DefaultCacheBehavior'
      { lambdaFunctionAssociations =
          Core.Nothing,
        allowedMethods = Core.Nothing,
        cachePolicyId = Core.Nothing,
        smoothStreaming = Core.Nothing,
        fieldLevelEncryptionId = Core.Nothing,
        originRequestPolicyId = Core.Nothing,
        maxTTL = Core.Nothing,
        forwardedValues = Core.Nothing,
        defaultTTL = Core.Nothing,
        realtimeLogConfigArn = Core.Nothing,
        minTTL = Core.Nothing,
        compress = Core.Nothing,
        trustedKeyGroups = Core.Nothing,
        trustedSigners = Core.Nothing,
        targetOriginId = pTargetOriginId_,
        viewerProtocolPolicy = pViewerProtocolPolicy_
      }

-- | A complex type that contains zero or more Lambda function associations
-- for a cache behavior.
defaultCacheBehavior_lambdaFunctionAssociations :: Lens.Lens' DefaultCacheBehavior (Core.Maybe LambdaFunctionAssociations)
defaultCacheBehavior_lambdaFunctionAssociations = Lens.lens (\DefaultCacheBehavior' {lambdaFunctionAssociations} -> lambdaFunctionAssociations) (\s@DefaultCacheBehavior' {} a -> s {lambdaFunctionAssociations = a} :: DefaultCacheBehavior)

-- | Undocumented member.
defaultCacheBehavior_allowedMethods :: Lens.Lens' DefaultCacheBehavior (Core.Maybe AllowedMethods)
defaultCacheBehavior_allowedMethods = Lens.lens (\DefaultCacheBehavior' {allowedMethods} -> allowedMethods) (\s@DefaultCacheBehavior' {} a -> s {allowedMethods = a} :: DefaultCacheBehavior)

-- | The unique identifier of the cache policy that is attached to the
-- default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_cachePolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
defaultCacheBehavior_cachePolicyId = Lens.lens (\DefaultCacheBehavior' {cachePolicyId} -> cachePolicyId) (\s@DefaultCacheBehavior' {} a -> s {cachePolicyId = a} :: DefaultCacheBehavior)

-- | Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
defaultCacheBehavior_smoothStreaming :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
defaultCacheBehavior_smoothStreaming = Lens.lens (\DefaultCacheBehavior' {smoothStreaming} -> smoothStreaming) (\s@DefaultCacheBehavior' {} a -> s {smoothStreaming = a} :: DefaultCacheBehavior)

-- | The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for the
-- default cache behavior.
defaultCacheBehavior_fieldLevelEncryptionId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
defaultCacheBehavior_fieldLevelEncryptionId = Lens.lens (\DefaultCacheBehavior' {fieldLevelEncryptionId} -> fieldLevelEncryptionId) (\s@DefaultCacheBehavior' {} a -> s {fieldLevelEncryptionId = a} :: DefaultCacheBehavior)

-- | The unique identifier of the origin request policy that is attached to
-- the default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_originRequestPolicyId :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
defaultCacheBehavior_originRequestPolicyId = Lens.lens (\DefaultCacheBehavior' {originRequestPolicyId} -> originRequestPolicyId) (\s@DefaultCacheBehavior' {} a -> s {originRequestPolicyId = a} :: DefaultCacheBehavior)

-- | This field is deprecated. We recommend that you use the @MaxTTL@ field
-- in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The maximum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. The value that you
-- specify applies only when your origin adds HTTP headers such as
-- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_maxTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
defaultCacheBehavior_maxTTL = Lens.lens (\DefaultCacheBehavior' {maxTTL} -> maxTTL) (\s@DefaultCacheBehavior' {} a -> s {maxTTL = a} :: DefaultCacheBehavior)

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/working-with-policies.html Working with policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to include values in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send values to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
defaultCacheBehavior_forwardedValues :: Lens.Lens' DefaultCacheBehavior (Core.Maybe ForwardedValues)
defaultCacheBehavior_forwardedValues = Lens.lens (\DefaultCacheBehavior' {forwardedValues} -> forwardedValues) (\s@DefaultCacheBehavior' {} a -> s {forwardedValues = a} :: DefaultCacheBehavior)

-- | This field is deprecated. We recommend that you use the @DefaultTTL@
-- field in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. The value that you
-- specify applies only when your origin does not add HTTP headers such as
-- @Cache-Control max-age@, @Cache-Control s-maxage@, and @Expires@ to
-- objects. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_defaultTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
defaultCacheBehavior_defaultTTL = Lens.lens (\DefaultCacheBehavior' {defaultTTL} -> defaultTTL) (\s@DefaultCacheBehavior' {} a -> s {defaultTTL = a} :: DefaultCacheBehavior)

-- | The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_realtimeLogConfigArn :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Text)
defaultCacheBehavior_realtimeLogConfigArn = Lens.lens (\DefaultCacheBehavior' {realtimeLogConfigArn} -> realtimeLogConfigArn) (\s@DefaultCacheBehavior' {} a -> s {realtimeLogConfigArn = a} :: DefaultCacheBehavior)

-- | This field is deprecated. We recommend that you use the @MinTTL@ field
-- in a cache policy instead of this field. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront forwards another request to your origin to
-- determine whether the object has been updated. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- You must specify @0@ for @MinTTL@ if you configure CloudFront to forward
-- all headers to your origin (under @Headers@, if you specify @1@ for
-- @Quantity@ and @*@ for @Name@).
defaultCacheBehavior_minTTL :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Integer)
defaultCacheBehavior_minTTL = Lens.lens (\DefaultCacheBehavior' {minTTL} -> minTTL) (\s@DefaultCacheBehavior' {} a -> s {minTTL = a} :: DefaultCacheBehavior)

-- | Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify @true@; if not, specify @false@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_compress :: Lens.Lens' DefaultCacheBehavior (Core.Maybe Core.Bool)
defaultCacheBehavior_compress = Lens.lens (\DefaultCacheBehavior' {compress} -> compress) (\s@DefaultCacheBehavior' {} a -> s {compress = a} :: DefaultCacheBehavior)

-- | A list of key groups that CloudFront can use to validate signed URLs or
-- signed cookies.
--
-- When a cache behavior contains trusted key groups, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with a private key whose
-- corresponding public key is in the key group. The signed URL or cookie
-- contains information about which public key CloudFront should use to
-- verify the signature. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_trustedKeyGroups :: Lens.Lens' DefaultCacheBehavior (Core.Maybe TrustedKeyGroups)
defaultCacheBehavior_trustedKeyGroups = Lens.lens (\DefaultCacheBehavior' {trustedKeyGroups} -> trustedKeyGroups) (\s@DefaultCacheBehavior' {} a -> s {trustedKeyGroups = a} :: DefaultCacheBehavior)

-- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of AWS account IDs whose public keys CloudFront can use to
-- validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in a trusted signer’s AWS account. The signed URL or
-- cookie contains information about which public key CloudFront should use
-- to verify the signature. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_trustedSigners :: Lens.Lens' DefaultCacheBehavior (Core.Maybe TrustedSigners)
defaultCacheBehavior_trustedSigners = Lens.lens (\DefaultCacheBehavior' {trustedSigners} -> trustedSigners) (\s@DefaultCacheBehavior' {} a -> s {trustedSigners = a} :: DefaultCacheBehavior)

-- | The value of @ID@ for the origin that you want CloudFront to route
-- requests to when they use the default cache behavior.
defaultCacheBehavior_targetOriginId :: Lens.Lens' DefaultCacheBehavior Core.Text
defaultCacheBehavior_targetOriginId = Lens.lens (\DefaultCacheBehavior' {targetOriginId} -> targetOriginId) (\s@DefaultCacheBehavior' {} a -> s {targetOriginId = a} :: DefaultCacheBehavior)

-- | The protocol that viewers can use to access the files in the origin
-- specified by @TargetOriginId@ when a request matches the path pattern in
-- @PathPattern@. You can specify the following options:
--
-- -   @allow-all@: Viewers can use HTTP or HTTPS.
--
-- -   @redirect-to-https@: If a viewer submits an HTTP request, CloudFront
--     returns an HTTP status code of 301 (Moved Permanently) to the viewer
--     along with the HTTPS URL. The viewer then resubmits the request
--     using the new URL.
--
-- -   @https-only@: If a viewer sends an HTTP request, CloudFront returns
--     an HTTP status code of 403 (Forbidden).
--
-- For more information about requiring the HTTPS protocol, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-https-viewers-to-cloudfront.html Requiring HTTPS Between Viewers and CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The only way to guarantee that viewers retrieve an object that was
-- fetched from the origin using HTTPS is never to use any other protocol
-- to fetch the object. If you have recently changed from HTTP to HTTPS, we
-- recommend that you clear your objects’ cache because cached objects are
-- protocol agnostic. That means that an edge location will return an
-- object from the cache regardless of whether the current request protocol
-- matches the protocol used previously. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_viewerProtocolPolicy :: Lens.Lens' DefaultCacheBehavior ViewerProtocolPolicy
defaultCacheBehavior_viewerProtocolPolicy = Lens.lens (\DefaultCacheBehavior' {viewerProtocolPolicy} -> viewerProtocolPolicy) (\s@DefaultCacheBehavior' {} a -> s {viewerProtocolPolicy = a} :: DefaultCacheBehavior)

instance Core.FromXML DefaultCacheBehavior where
  parseXML x =
    DefaultCacheBehavior'
      Core.<$> (x Core..@? "LambdaFunctionAssociations")
      Core.<*> (x Core..@? "AllowedMethods")
      Core.<*> (x Core..@? "CachePolicyId")
      Core.<*> (x Core..@? "SmoothStreaming")
      Core.<*> (x Core..@? "FieldLevelEncryptionId")
      Core.<*> (x Core..@? "OriginRequestPolicyId")
      Core.<*> (x Core..@? "MaxTTL")
      Core.<*> (x Core..@? "ForwardedValues")
      Core.<*> (x Core..@? "DefaultTTL")
      Core.<*> (x Core..@? "RealtimeLogConfigArn")
      Core.<*> (x Core..@? "MinTTL")
      Core.<*> (x Core..@? "Compress")
      Core.<*> (x Core..@? "TrustedKeyGroups")
      Core.<*> (x Core..@? "TrustedSigners")
      Core.<*> (x Core..@ "TargetOriginId")
      Core.<*> (x Core..@ "ViewerProtocolPolicy")

instance Core.Hashable DefaultCacheBehavior

instance Core.NFData DefaultCacheBehavior

instance Core.ToXML DefaultCacheBehavior where
  toXML DefaultCacheBehavior' {..} =
    Core.mconcat
      [ "LambdaFunctionAssociations"
          Core.@= lambdaFunctionAssociations,
        "AllowedMethods" Core.@= allowedMethods,
        "CachePolicyId" Core.@= cachePolicyId,
        "SmoothStreaming" Core.@= smoothStreaming,
        "FieldLevelEncryptionId"
          Core.@= fieldLevelEncryptionId,
        "OriginRequestPolicyId"
          Core.@= originRequestPolicyId,
        "MaxTTL" Core.@= maxTTL,
        "ForwardedValues" Core.@= forwardedValues,
        "DefaultTTL" Core.@= defaultTTL,
        "RealtimeLogConfigArn" Core.@= realtimeLogConfigArn,
        "MinTTL" Core.@= minTTL,
        "Compress" Core.@= compress,
        "TrustedKeyGroups" Core.@= trustedKeyGroups,
        "TrustedSigners" Core.@= trustedSigners,
        "TargetOriginId" Core.@= targetOriginId,
        "ViewerProtocolPolicy" Core.@= viewerProtocolPolicy
      ]
