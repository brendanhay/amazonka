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
-- Module      : Amazonka.CloudFront.Types.DefaultCacheBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.DefaultCacheBehavior where

import Amazonka.CloudFront.Types.AllowedMethods
import Amazonka.CloudFront.Types.ForwardedValues
import Amazonka.CloudFront.Types.FunctionAssociations
import Amazonka.CloudFront.Types.LambdaFunctionAssociations
import Amazonka.CloudFront.Types.TrustedKeyGroups
import Amazonka.CloudFront.Types.TrustedSigners
import Amazonka.CloudFront.Types.ViewerProtocolPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type that describes the default cache behavior if you don’t
-- specify a @CacheBehavior@ element or if request URLs don’t match any of
-- the values of @PathPattern@ in @CacheBehavior@ elements. You must create
-- exactly one default cache behavior.
--
-- /See:/ 'newDefaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
  { -- | A list of key groups that CloudFront can use to validate signed URLs or
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
    trustedKeyGroups :: Prelude.Maybe TrustedKeyGroups,
    allowedMethods :: Prelude.Maybe AllowedMethods,
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
    defaultTTL :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier of the cache policy that is attached to the
    -- default cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
    -- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
    cachePolicyId :: Prelude.Maybe Prelude.Text,
    -- | The value of @ID@ for the field-level encryption configuration that you
    -- want CloudFront to use for encrypting specific fields of data for the
    -- default cache behavior.
    fieldLevelEncryptionId :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains zero or more Lambda\@Edge function
    -- associations for a cache behavior.
    lambdaFunctionAssociations :: Prelude.Maybe LambdaFunctionAssociations,
    -- | The unique identifier of the origin request policy that is attached to
    -- the default cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    originRequestPolicyId :: Prelude.Maybe Prelude.Text,
    -- | A list of CloudFront functions that are associated with this cache
    -- behavior. CloudFront functions must be published to the @LIVE@ stage to
    -- associate them with a cache behavior.
    functionAssociations :: Prelude.Maybe FunctionAssociations,
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
    -- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
    -- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
    --
    -- A complex type that specifies how CloudFront handles query strings,
    -- cookies, and HTTP headers.
    forwardedValues :: Prelude.Maybe ForwardedValues,
    -- | The identifier for a response headers policy.
    responseHeadersPolicyId :: Prelude.Maybe Prelude.Text,
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
    minTTL :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration that
    -- is attached to this cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
    -- in the /Amazon CloudFront Developer Guide/.
    realtimeLogConfigArn :: Prelude.Maybe Prelude.Text,
    -- | Whether you want CloudFront to automatically compress certain files for
    -- this cache behavior. If so, specify @true@; if not, specify @false@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
    -- in the /Amazon CloudFront Developer Guide/.
    compress :: Prelude.Maybe Prelude.Bool,
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
    maxTTL :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether you want to distribute media files in the Microsoft
    -- Smooth Streaming format using the origin that is associated with this
    -- cache behavior. If so, specify @true@; if not, specify @false@. If you
    -- specify @true@ for @SmoothStreaming@, you can still distribute other
    -- content using this cache behavior if the content matches the value of
    -- @PathPattern@.
    smoothStreaming :: Prelude.Maybe Prelude.Bool,
    -- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
    --
    -- A list of Amazon Web Services account IDs whose public keys CloudFront
    -- can use to validate signed URLs or signed cookies.
    --
    -- When a cache behavior contains trusted signers, CloudFront requires
    -- signed URLs or signed cookies for all requests that match the cache
    -- behavior. The URLs or cookies must be signed with the private key of a
    -- CloudFront key pair in a trusted signer’s Amazon Web Services account.
    -- The signed URL or cookie contains information about which public key
    -- CloudFront should use to verify the signature. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedSigners :: Prelude.Maybe TrustedSigners,
    -- | The value of @ID@ for the origin that you want CloudFront to route
    -- requests to when they use the default cache behavior.
    targetOriginId :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultCacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'allowedMethods', 'defaultCacheBehavior_allowedMethods' - Undocumented member.
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
-- 'cachePolicyId', 'defaultCacheBehavior_cachePolicyId' - The unique identifier of the cache policy that is attached to the
-- default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- 'fieldLevelEncryptionId', 'defaultCacheBehavior_fieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for the
-- default cache behavior.
--
-- 'lambdaFunctionAssociations', 'defaultCacheBehavior_lambdaFunctionAssociations' - A complex type that contains zero or more Lambda\@Edge function
-- associations for a cache behavior.
--
-- 'originRequestPolicyId', 'defaultCacheBehavior_originRequestPolicyId' - The unique identifier of the origin request policy that is attached to
-- the default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'functionAssociations', 'defaultCacheBehavior_functionAssociations' - A list of CloudFront functions that are associated with this cache
-- behavior. CloudFront functions must be published to the @LIVE@ stage to
-- associate them with a cache behavior.
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
-- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
--
-- 'responseHeadersPolicyId', 'defaultCacheBehavior_responseHeadersPolicyId' - The identifier for a response headers policy.
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
-- 'realtimeLogConfigArn', 'defaultCacheBehavior_realtimeLogConfigArn' - The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'compress', 'defaultCacheBehavior_compress' - Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify @true@; if not, specify @false@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
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
-- 'smoothStreaming', 'defaultCacheBehavior_smoothStreaming' - Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
--
-- 'trustedSigners', 'defaultCacheBehavior_trustedSigners' - We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of Amazon Web Services account IDs whose public keys CloudFront
-- can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in a trusted signer’s Amazon Web Services account.
-- The signed URL or cookie contains information about which public key
-- CloudFront should use to verify the signature. For more information, see
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
  Prelude.Text ->
  -- | 'viewerProtocolPolicy'
  ViewerProtocolPolicy ->
  DefaultCacheBehavior
newDefaultCacheBehavior
  pTargetOriginId_
  pViewerProtocolPolicy_ =
    DefaultCacheBehavior'
      { trustedKeyGroups =
          Prelude.Nothing,
        allowedMethods = Prelude.Nothing,
        defaultTTL = Prelude.Nothing,
        cachePolicyId = Prelude.Nothing,
        fieldLevelEncryptionId = Prelude.Nothing,
        lambdaFunctionAssociations = Prelude.Nothing,
        originRequestPolicyId = Prelude.Nothing,
        functionAssociations = Prelude.Nothing,
        forwardedValues = Prelude.Nothing,
        responseHeadersPolicyId = Prelude.Nothing,
        minTTL = Prelude.Nothing,
        realtimeLogConfigArn = Prelude.Nothing,
        compress = Prelude.Nothing,
        maxTTL = Prelude.Nothing,
        smoothStreaming = Prelude.Nothing,
        trustedSigners = Prelude.Nothing,
        targetOriginId = pTargetOriginId_,
        viewerProtocolPolicy = pViewerProtocolPolicy_
      }

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
defaultCacheBehavior_trustedKeyGroups :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe TrustedKeyGroups)
defaultCacheBehavior_trustedKeyGroups = Lens.lens (\DefaultCacheBehavior' {trustedKeyGroups} -> trustedKeyGroups) (\s@DefaultCacheBehavior' {} a -> s {trustedKeyGroups = a} :: DefaultCacheBehavior)

-- | Undocumented member.
defaultCacheBehavior_allowedMethods :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe AllowedMethods)
defaultCacheBehavior_allowedMethods = Lens.lens (\DefaultCacheBehavior' {allowedMethods} -> allowedMethods) (\s@DefaultCacheBehavior' {} a -> s {allowedMethods = a} :: DefaultCacheBehavior)

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
defaultCacheBehavior_defaultTTL :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Integer)
defaultCacheBehavior_defaultTTL = Lens.lens (\DefaultCacheBehavior' {defaultTTL} -> defaultTTL) (\s@DefaultCacheBehavior' {} a -> s {defaultTTL = a} :: DefaultCacheBehavior)

-- | The unique identifier of the cache policy that is attached to the
-- default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
defaultCacheBehavior_cachePolicyId :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Text)
defaultCacheBehavior_cachePolicyId = Lens.lens (\DefaultCacheBehavior' {cachePolicyId} -> cachePolicyId) (\s@DefaultCacheBehavior' {} a -> s {cachePolicyId = a} :: DefaultCacheBehavior)

-- | The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for the
-- default cache behavior.
defaultCacheBehavior_fieldLevelEncryptionId :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Text)
defaultCacheBehavior_fieldLevelEncryptionId = Lens.lens (\DefaultCacheBehavior' {fieldLevelEncryptionId} -> fieldLevelEncryptionId) (\s@DefaultCacheBehavior' {} a -> s {fieldLevelEncryptionId = a} :: DefaultCacheBehavior)

-- | A complex type that contains zero or more Lambda\@Edge function
-- associations for a cache behavior.
defaultCacheBehavior_lambdaFunctionAssociations :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe LambdaFunctionAssociations)
defaultCacheBehavior_lambdaFunctionAssociations = Lens.lens (\DefaultCacheBehavior' {lambdaFunctionAssociations} -> lambdaFunctionAssociations) (\s@DefaultCacheBehavior' {} a -> s {lambdaFunctionAssociations = a} :: DefaultCacheBehavior)

-- | The unique identifier of the origin request policy that is attached to
-- the default cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_originRequestPolicyId :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Text)
defaultCacheBehavior_originRequestPolicyId = Lens.lens (\DefaultCacheBehavior' {originRequestPolicyId} -> originRequestPolicyId) (\s@DefaultCacheBehavior' {} a -> s {originRequestPolicyId = a} :: DefaultCacheBehavior)

-- | A list of CloudFront functions that are associated with this cache
-- behavior. CloudFront functions must be published to the @LIVE@ stage to
-- associate them with a cache behavior.
defaultCacheBehavior_functionAssociations :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe FunctionAssociations)
defaultCacheBehavior_functionAssociations = Lens.lens (\DefaultCacheBehavior' {functionAssociations} -> functionAssociations) (\s@DefaultCacheBehavior' {} a -> s {functionAssociations = a} :: DefaultCacheBehavior)

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
-- A @DefaultCacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
defaultCacheBehavior_forwardedValues :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe ForwardedValues)
defaultCacheBehavior_forwardedValues = Lens.lens (\DefaultCacheBehavior' {forwardedValues} -> forwardedValues) (\s@DefaultCacheBehavior' {} a -> s {forwardedValues = a} :: DefaultCacheBehavior)

-- | The identifier for a response headers policy.
defaultCacheBehavior_responseHeadersPolicyId :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Text)
defaultCacheBehavior_responseHeadersPolicyId = Lens.lens (\DefaultCacheBehavior' {responseHeadersPolicyId} -> responseHeadersPolicyId) (\s@DefaultCacheBehavior' {} a -> s {responseHeadersPolicyId = a} :: DefaultCacheBehavior)

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
defaultCacheBehavior_minTTL :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Integer)
defaultCacheBehavior_minTTL = Lens.lens (\DefaultCacheBehavior' {minTTL} -> minTTL) (\s@DefaultCacheBehavior' {} a -> s {minTTL = a} :: DefaultCacheBehavior)

-- | The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_realtimeLogConfigArn :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Text)
defaultCacheBehavior_realtimeLogConfigArn = Lens.lens (\DefaultCacheBehavior' {realtimeLogConfigArn} -> realtimeLogConfigArn) (\s@DefaultCacheBehavior' {} a -> s {realtimeLogConfigArn = a} :: DefaultCacheBehavior)

-- | Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify @true@; if not, specify @false@. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_compress :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Bool)
defaultCacheBehavior_compress = Lens.lens (\DefaultCacheBehavior' {compress} -> compress) (\s@DefaultCacheBehavior' {} a -> s {compress = a} :: DefaultCacheBehavior)

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
defaultCacheBehavior_maxTTL :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Integer)
defaultCacheBehavior_maxTTL = Lens.lens (\DefaultCacheBehavior' {maxTTL} -> maxTTL) (\s@DefaultCacheBehavior' {} a -> s {maxTTL = a} :: DefaultCacheBehavior)

-- | Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
defaultCacheBehavior_smoothStreaming :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe Prelude.Bool)
defaultCacheBehavior_smoothStreaming = Lens.lens (\DefaultCacheBehavior' {smoothStreaming} -> smoothStreaming) (\s@DefaultCacheBehavior' {} a -> s {smoothStreaming = a} :: DefaultCacheBehavior)

-- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of Amazon Web Services account IDs whose public keys CloudFront
-- can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in a trusted signer’s Amazon Web Services account.
-- The signed URL or cookie contains information about which public key
-- CloudFront should use to verify the signature. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
defaultCacheBehavior_trustedSigners :: Lens.Lens' DefaultCacheBehavior (Prelude.Maybe TrustedSigners)
defaultCacheBehavior_trustedSigners = Lens.lens (\DefaultCacheBehavior' {trustedSigners} -> trustedSigners) (\s@DefaultCacheBehavior' {} a -> s {trustedSigners = a} :: DefaultCacheBehavior)

-- | The value of @ID@ for the origin that you want CloudFront to route
-- requests to when they use the default cache behavior.
defaultCacheBehavior_targetOriginId :: Lens.Lens' DefaultCacheBehavior Prelude.Text
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
      Prelude.<$> (x Core..@? "TrustedKeyGroups")
      Prelude.<*> (x Core..@? "AllowedMethods")
      Prelude.<*> (x Core..@? "DefaultTTL")
      Prelude.<*> (x Core..@? "CachePolicyId")
      Prelude.<*> (x Core..@? "FieldLevelEncryptionId")
      Prelude.<*> (x Core..@? "LambdaFunctionAssociations")
      Prelude.<*> (x Core..@? "OriginRequestPolicyId")
      Prelude.<*> (x Core..@? "FunctionAssociations")
      Prelude.<*> (x Core..@? "ForwardedValues")
      Prelude.<*> (x Core..@? "ResponseHeadersPolicyId")
      Prelude.<*> (x Core..@? "MinTTL")
      Prelude.<*> (x Core..@? "RealtimeLogConfigArn")
      Prelude.<*> (x Core..@? "Compress")
      Prelude.<*> (x Core..@? "MaxTTL")
      Prelude.<*> (x Core..@? "SmoothStreaming")
      Prelude.<*> (x Core..@? "TrustedSigners")
      Prelude.<*> (x Core..@ "TargetOriginId")
      Prelude.<*> (x Core..@ "ViewerProtocolPolicy")

instance Prelude.Hashable DefaultCacheBehavior where
  hashWithSalt _salt DefaultCacheBehavior' {..} =
    _salt `Prelude.hashWithSalt` trustedKeyGroups
      `Prelude.hashWithSalt` allowedMethods
      `Prelude.hashWithSalt` defaultTTL
      `Prelude.hashWithSalt` cachePolicyId
      `Prelude.hashWithSalt` fieldLevelEncryptionId
      `Prelude.hashWithSalt` lambdaFunctionAssociations
      `Prelude.hashWithSalt` originRequestPolicyId
      `Prelude.hashWithSalt` functionAssociations
      `Prelude.hashWithSalt` forwardedValues
      `Prelude.hashWithSalt` responseHeadersPolicyId
      `Prelude.hashWithSalt` minTTL
      `Prelude.hashWithSalt` realtimeLogConfigArn
      `Prelude.hashWithSalt` compress
      `Prelude.hashWithSalt` maxTTL
      `Prelude.hashWithSalt` smoothStreaming
      `Prelude.hashWithSalt` trustedSigners
      `Prelude.hashWithSalt` targetOriginId
      `Prelude.hashWithSalt` viewerProtocolPolicy

instance Prelude.NFData DefaultCacheBehavior where
  rnf DefaultCacheBehavior' {..} =
    Prelude.rnf trustedKeyGroups
      `Prelude.seq` Prelude.rnf allowedMethods
      `Prelude.seq` Prelude.rnf defaultTTL
      `Prelude.seq` Prelude.rnf cachePolicyId
      `Prelude.seq` Prelude.rnf fieldLevelEncryptionId
      `Prelude.seq` Prelude.rnf lambdaFunctionAssociations
      `Prelude.seq` Prelude.rnf originRequestPolicyId
      `Prelude.seq` Prelude.rnf functionAssociations
      `Prelude.seq` Prelude.rnf forwardedValues
      `Prelude.seq` Prelude.rnf responseHeadersPolicyId
      `Prelude.seq` Prelude.rnf minTTL
      `Prelude.seq` Prelude.rnf realtimeLogConfigArn
      `Prelude.seq` Prelude.rnf compress
      `Prelude.seq` Prelude.rnf maxTTL
      `Prelude.seq` Prelude.rnf smoothStreaming
      `Prelude.seq` Prelude.rnf trustedSigners
      `Prelude.seq` Prelude.rnf targetOriginId
      `Prelude.seq` Prelude.rnf viewerProtocolPolicy

instance Core.ToXML DefaultCacheBehavior where
  toXML DefaultCacheBehavior' {..} =
    Prelude.mconcat
      [ "TrustedKeyGroups" Core.@= trustedKeyGroups,
        "AllowedMethods" Core.@= allowedMethods,
        "DefaultTTL" Core.@= defaultTTL,
        "CachePolicyId" Core.@= cachePolicyId,
        "FieldLevelEncryptionId"
          Core.@= fieldLevelEncryptionId,
        "LambdaFunctionAssociations"
          Core.@= lambdaFunctionAssociations,
        "OriginRequestPolicyId"
          Core.@= originRequestPolicyId,
        "FunctionAssociations" Core.@= functionAssociations,
        "ForwardedValues" Core.@= forwardedValues,
        "ResponseHeadersPolicyId"
          Core.@= responseHeadersPolicyId,
        "MinTTL" Core.@= minTTL,
        "RealtimeLogConfigArn" Core.@= realtimeLogConfigArn,
        "Compress" Core.@= compress,
        "MaxTTL" Core.@= maxTTL,
        "SmoothStreaming" Core.@= smoothStreaming,
        "TrustedSigners" Core.@= trustedSigners,
        "TargetOriginId" Core.@= targetOriginId,
        "ViewerProtocolPolicy" Core.@= viewerProtocolPolicy
      ]
