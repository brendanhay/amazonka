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
-- Module      : Amazonka.CloudFront.Types.CacheBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CacheBehavior where

import Amazonka.CloudFront.Types.AllowedMethods
import Amazonka.CloudFront.Types.ForwardedValues
import Amazonka.CloudFront.Types.FunctionAssociations
import Amazonka.CloudFront.Types.LambdaFunctionAssociations
import Amazonka.CloudFront.Types.TrustedKeyGroups
import Amazonka.CloudFront.Types.TrustedSigners
import Amazonka.CloudFront.Types.ViewerProtocolPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that describes how CloudFront processes requests.
--
-- You must create at least as many cache behaviors (including the default
-- cache behavior) as you have origins if you want CloudFront to serve
-- objects from all of the origins. Each cache behavior specifies the one
-- origin from which you want CloudFront to get objects. If you have two
-- origins and only the default cache behavior, the default cache behavior
-- will cause CloudFront to get objects from one of the origins, but the
-- other origin is never used.
--
-- For the current quota (formerly known as limit) on the number of cache
-- behaviors that you can add to a distribution, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you don\'t want to specify any cache behaviors, include only an empty
-- @CacheBehaviors@ element. Don\'t include an empty @CacheBehavior@
-- element because this is invalid.
--
-- To delete all cache behaviors in an existing distribution, update the
-- distribution configuration and include only an empty @CacheBehaviors@
-- element.
--
-- To add, change, or remove one or more cache behaviors, update the
-- distribution configuration and specify all of the cache behaviors that
-- you want to include in the updated distribution.
--
-- For more information about cache behaviors, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newCacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { allowedMethods :: Prelude.Maybe AllowedMethods,
    -- | The unique identifier of the cache policy that is attached to this cache
    -- behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- A @CacheBehavior@ must include either a @CachePolicyId@ or
    -- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
    cachePolicyId :: Prelude.Maybe Prelude.Text,
    -- | Whether you want CloudFront to automatically compress certain files for
    -- this cache behavior. If so, specify true; if not, specify false. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
    -- in the /Amazon CloudFront Developer Guide/.
    compress :: Prelude.Maybe Prelude.Bool,
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
    -- | The value of @ID@ for the field-level encryption configuration that you
    -- want CloudFront to use for encrypting specific fields of data for this
    -- cache behavior.
    fieldLevelEncryptionId :: Prelude.Maybe Prelude.Text,
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
    -- A @CacheBehavior@ must include either a @CachePolicyId@ or
    -- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
    --
    -- A complex type that specifies how CloudFront handles query strings,
    -- cookies, and HTTP headers.
    forwardedValues :: Prelude.Maybe ForwardedValues,
    -- | A list of CloudFront functions that are associated with this cache
    -- behavior. CloudFront functions must be published to the @LIVE@ stage to
    -- associate them with a cache behavior.
    functionAssociations :: Prelude.Maybe FunctionAssociations,
    -- | A complex type that contains zero or more Lambda\@Edge function
    -- associations for a cache behavior.
    lambdaFunctionAssociations :: Prelude.Maybe LambdaFunctionAssociations,
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
    -- | The unique identifier of the origin request policy that is attached to
    -- this cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- or
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    originRequestPolicyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration that
    -- is attached to this cache behavior. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
    -- in the /Amazon CloudFront Developer Guide/.
    realtimeLogConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for a response headers policy.
    responseHeadersPolicyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether you want to distribute media files in the Microsoft
    -- Smooth Streaming format using the origin that is associated with this
    -- cache behavior. If so, specify @true@; if not, specify @false@. If you
    -- specify @true@ for @SmoothStreaming@, you can still distribute other
    -- content using this cache behavior if the content matches the value of
    -- @PathPattern@.
    smoothStreaming :: Prelude.Maybe Prelude.Bool,
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
    trustedKeyGroups :: Prelude.Maybe TrustedKeyGroups,
    -- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
    --
    -- A list of Amazon Web Services account IDs whose public keys CloudFront
    -- can use to validate signed URLs or signed cookies.
    --
    -- When a cache behavior contains trusted signers, CloudFront requires
    -- signed URLs or signed cookies for all requests that match the cache
    -- behavior. The URLs or cookies must be signed with the private key of a
    -- CloudFront key pair in the trusted signer\'s Amazon Web Services
    -- account. The signed URL or cookie contains information about which
    -- public key CloudFront should use to verify the signature. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedSigners :: Prelude.Maybe TrustedSigners,
    -- | The pattern (for example, @images\/*.jpg@) that specifies which requests
    -- to apply the behavior to. When CloudFront receives a viewer request, the
    -- requested path is compared with path patterns in the order in which
    -- cache behaviors are listed in the distribution.
    --
    -- You can optionally include a slash (@\/@) at the beginning of the path
    -- pattern. For example, @\/images\/*.jpg@. CloudFront behavior is the same
    -- with or without the leading @\/@.
    --
    -- The path pattern for the default cache behavior is @*@ and cannot be
    -- changed. If the request for an object does not match the path pattern
    -- for any cache behaviors, CloudFront applies the behavior in the default
    -- cache behavior.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern>
    -- in the /Amazon CloudFront Developer Guide/.
    pathPattern :: Prelude.Text,
    -- | The value of @ID@ for the origin that you want CloudFront to route
    -- requests to when they match this cache behavior.
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
    -- recommend that you clear your objects\' cache because cached objects are
    -- protocol agnostic. That means that an edge location will return an
    -- object from the cache regardless of whether the current request protocol
    -- matches the protocol used previously. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
    -- in the /Amazon CloudFront Developer Guide/.
    viewerProtocolPolicy :: ViewerProtocolPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedMethods', 'cacheBehavior_allowedMethods' - Undocumented member.
--
-- 'cachePolicyId', 'cacheBehavior_cachePolicyId' - The unique identifier of the cache policy that is attached to this cache
-- behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A @CacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- 'compress', 'cacheBehavior_compress' - Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify true; if not, specify false. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'defaultTTL', 'cacheBehavior_defaultTTL' - This field is deprecated. We recommend that you use the @DefaultTTL@
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
-- 'fieldLevelEncryptionId', 'cacheBehavior_fieldLevelEncryptionId' - The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for this
-- cache behavior.
--
-- 'forwardedValues', 'cacheBehavior_forwardedValues' - This field is deprecated. We recommend that you use a cache policy or an
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
-- A @CacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
--
-- 'functionAssociations', 'cacheBehavior_functionAssociations' - A list of CloudFront functions that are associated with this cache
-- behavior. CloudFront functions must be published to the @LIVE@ stage to
-- associate them with a cache behavior.
--
-- 'lambdaFunctionAssociations', 'cacheBehavior_lambdaFunctionAssociations' - A complex type that contains zero or more Lambda\@Edge function
-- associations for a cache behavior.
--
-- 'maxTTL', 'cacheBehavior_maxTTL' - This field is deprecated. We recommend that you use the @MaxTTL@ field
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
-- 'minTTL', 'cacheBehavior_minTTL' - This field is deprecated. We recommend that you use the @MinTTL@ field
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
-- 'originRequestPolicyId', 'cacheBehavior_originRequestPolicyId' - The unique identifier of the origin request policy that is attached to
-- this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'realtimeLogConfigArn', 'cacheBehavior_realtimeLogConfigArn' - The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'responseHeadersPolicyId', 'cacheBehavior_responseHeadersPolicyId' - The identifier for a response headers policy.
--
-- 'smoothStreaming', 'cacheBehavior_smoothStreaming' - Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
--
-- 'trustedKeyGroups', 'cacheBehavior_trustedKeyGroups' - A list of key groups that CloudFront can use to validate signed URLs or
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
-- 'trustedSigners', 'cacheBehavior_trustedSigners' - We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of Amazon Web Services account IDs whose public keys CloudFront
-- can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in the trusted signer\'s Amazon Web Services
-- account. The signed URL or cookie contains information about which
-- public key CloudFront should use to verify the signature. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'pathPattern', 'cacheBehavior_pathPattern' - The pattern (for example, @images\/*.jpg@) that specifies which requests
-- to apply the behavior to. When CloudFront receives a viewer request, the
-- requested path is compared with path patterns in the order in which
-- cache behaviors are listed in the distribution.
--
-- You can optionally include a slash (@\/@) at the beginning of the path
-- pattern. For example, @\/images\/*.jpg@. CloudFront behavior is the same
-- with or without the leading @\/@.
--
-- The path pattern for the default cache behavior is @*@ and cannot be
-- changed. If the request for an object does not match the path pattern
-- for any cache behaviors, CloudFront applies the behavior in the default
-- cache behavior.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'targetOriginId', 'cacheBehavior_targetOriginId' - The value of @ID@ for the origin that you want CloudFront to route
-- requests to when they match this cache behavior.
--
-- 'viewerProtocolPolicy', 'cacheBehavior_viewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin
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
-- recommend that you clear your objects\' cache because cached objects are
-- protocol agnostic. That means that an edge location will return an
-- object from the cache regardless of whether the current request protocol
-- matches the protocol used previously. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
-- in the /Amazon CloudFront Developer Guide/.
newCacheBehavior ::
  -- | 'pathPattern'
  Prelude.Text ->
  -- | 'targetOriginId'
  Prelude.Text ->
  -- | 'viewerProtocolPolicy'
  ViewerProtocolPolicy ->
  CacheBehavior
newCacheBehavior
  pPathPattern_
  pTargetOriginId_
  pViewerProtocolPolicy_ =
    CacheBehavior'
      { allowedMethods = Prelude.Nothing,
        cachePolicyId = Prelude.Nothing,
        compress = Prelude.Nothing,
        defaultTTL = Prelude.Nothing,
        fieldLevelEncryptionId = Prelude.Nothing,
        forwardedValues = Prelude.Nothing,
        functionAssociations = Prelude.Nothing,
        lambdaFunctionAssociations = Prelude.Nothing,
        maxTTL = Prelude.Nothing,
        minTTL = Prelude.Nothing,
        originRequestPolicyId = Prelude.Nothing,
        realtimeLogConfigArn = Prelude.Nothing,
        responseHeadersPolicyId = Prelude.Nothing,
        smoothStreaming = Prelude.Nothing,
        trustedKeyGroups = Prelude.Nothing,
        trustedSigners = Prelude.Nothing,
        pathPattern = pPathPattern_,
        targetOriginId = pTargetOriginId_,
        viewerProtocolPolicy = pViewerProtocolPolicy_
      }

-- | Undocumented member.
cacheBehavior_allowedMethods :: Lens.Lens' CacheBehavior (Prelude.Maybe AllowedMethods)
cacheBehavior_allowedMethods = Lens.lens (\CacheBehavior' {allowedMethods} -> allowedMethods) (\s@CacheBehavior' {} a -> s {allowedMethods = a} :: CacheBehavior)

-- | The unique identifier of the cache policy that is attached to this cache
-- behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html Using the managed cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- A @CacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
cacheBehavior_cachePolicyId :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Text)
cacheBehavior_cachePolicyId = Lens.lens (\CacheBehavior' {cachePolicyId} -> cachePolicyId) (\s@CacheBehavior' {} a -> s {cachePolicyId = a} :: CacheBehavior)

-- | Whether you want CloudFront to automatically compress certain files for
-- this cache behavior. If so, specify true; if not, specify false. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_compress :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Bool)
cacheBehavior_compress = Lens.lens (\CacheBehavior' {compress} -> compress) (\s@CacheBehavior' {} a -> s {compress = a} :: CacheBehavior)

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
cacheBehavior_defaultTTL :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Integer)
cacheBehavior_defaultTTL = Lens.lens (\CacheBehavior' {defaultTTL} -> defaultTTL) (\s@CacheBehavior' {} a -> s {defaultTTL = a} :: CacheBehavior)

-- | The value of @ID@ for the field-level encryption configuration that you
-- want CloudFront to use for encrypting specific fields of data for this
-- cache behavior.
cacheBehavior_fieldLevelEncryptionId :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Text)
cacheBehavior_fieldLevelEncryptionId = Lens.lens (\CacheBehavior' {fieldLevelEncryptionId} -> fieldLevelEncryptionId) (\s@CacheBehavior' {} a -> s {fieldLevelEncryptionId = a} :: CacheBehavior)

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
-- A @CacheBehavior@ must include either a @CachePolicyId@ or
-- @ForwardedValues@. We recommend that you use a @CachePolicyId@.
--
-- A complex type that specifies how CloudFront handles query strings,
-- cookies, and HTTP headers.
cacheBehavior_forwardedValues :: Lens.Lens' CacheBehavior (Prelude.Maybe ForwardedValues)
cacheBehavior_forwardedValues = Lens.lens (\CacheBehavior' {forwardedValues} -> forwardedValues) (\s@CacheBehavior' {} a -> s {forwardedValues = a} :: CacheBehavior)

-- | A list of CloudFront functions that are associated with this cache
-- behavior. CloudFront functions must be published to the @LIVE@ stage to
-- associate them with a cache behavior.
cacheBehavior_functionAssociations :: Lens.Lens' CacheBehavior (Prelude.Maybe FunctionAssociations)
cacheBehavior_functionAssociations = Lens.lens (\CacheBehavior' {functionAssociations} -> functionAssociations) (\s@CacheBehavior' {} a -> s {functionAssociations = a} :: CacheBehavior)

-- | A complex type that contains zero or more Lambda\@Edge function
-- associations for a cache behavior.
cacheBehavior_lambdaFunctionAssociations :: Lens.Lens' CacheBehavior (Prelude.Maybe LambdaFunctionAssociations)
cacheBehavior_lambdaFunctionAssociations = Lens.lens (\CacheBehavior' {lambdaFunctionAssociations} -> lambdaFunctionAssociations) (\s@CacheBehavior' {} a -> s {lambdaFunctionAssociations = a} :: CacheBehavior)

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
cacheBehavior_maxTTL :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Integer)
cacheBehavior_maxTTL = Lens.lens (\CacheBehavior' {maxTTL} -> maxTTL) (\s@CacheBehavior' {} a -> s {maxTTL = a} :: CacheBehavior)

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
cacheBehavior_minTTL :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Integer)
cacheBehavior_minTTL = Lens.lens (\CacheBehavior' {minTTL} -> minTTL) (\s@CacheBehavior' {} a -> s {minTTL = a} :: CacheBehavior)

-- | The unique identifier of the origin request policy that is attached to
-- this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-origin-request-policies.html Using the managed origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_originRequestPolicyId :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Text)
cacheBehavior_originRequestPolicyId = Lens.lens (\CacheBehavior' {originRequestPolicyId} -> originRequestPolicyId) (\s@CacheBehavior' {} a -> s {originRequestPolicyId = a} :: CacheBehavior)

-- | The Amazon Resource Name (ARN) of the real-time log configuration that
-- is attached to this cache behavior. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_realtimeLogConfigArn :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Text)
cacheBehavior_realtimeLogConfigArn = Lens.lens (\CacheBehavior' {realtimeLogConfigArn} -> realtimeLogConfigArn) (\s@CacheBehavior' {} a -> s {realtimeLogConfigArn = a} :: CacheBehavior)

-- | The identifier for a response headers policy.
cacheBehavior_responseHeadersPolicyId :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Text)
cacheBehavior_responseHeadersPolicyId = Lens.lens (\CacheBehavior' {responseHeadersPolicyId} -> responseHeadersPolicyId) (\s@CacheBehavior' {} a -> s {responseHeadersPolicyId = a} :: CacheBehavior)

-- | Indicates whether you want to distribute media files in the Microsoft
-- Smooth Streaming format using the origin that is associated with this
-- cache behavior. If so, specify @true@; if not, specify @false@. If you
-- specify @true@ for @SmoothStreaming@, you can still distribute other
-- content using this cache behavior if the content matches the value of
-- @PathPattern@.
cacheBehavior_smoothStreaming :: Lens.Lens' CacheBehavior (Prelude.Maybe Prelude.Bool)
cacheBehavior_smoothStreaming = Lens.lens (\CacheBehavior' {smoothStreaming} -> smoothStreaming) (\s@CacheBehavior' {} a -> s {smoothStreaming = a} :: CacheBehavior)

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
cacheBehavior_trustedKeyGroups :: Lens.Lens' CacheBehavior (Prelude.Maybe TrustedKeyGroups)
cacheBehavior_trustedKeyGroups = Lens.lens (\CacheBehavior' {trustedKeyGroups} -> trustedKeyGroups) (\s@CacheBehavior' {} a -> s {trustedKeyGroups = a} :: CacheBehavior)

-- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- A list of Amazon Web Services account IDs whose public keys CloudFront
-- can use to validate signed URLs or signed cookies.
--
-- When a cache behavior contains trusted signers, CloudFront requires
-- signed URLs or signed cookies for all requests that match the cache
-- behavior. The URLs or cookies must be signed with the private key of a
-- CloudFront key pair in the trusted signer\'s Amazon Web Services
-- account. The signed URL or cookie contains information about which
-- public key CloudFront should use to verify the signature. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving private content>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_trustedSigners :: Lens.Lens' CacheBehavior (Prelude.Maybe TrustedSigners)
cacheBehavior_trustedSigners = Lens.lens (\CacheBehavior' {trustedSigners} -> trustedSigners) (\s@CacheBehavior' {} a -> s {trustedSigners = a} :: CacheBehavior)

-- | The pattern (for example, @images\/*.jpg@) that specifies which requests
-- to apply the behavior to. When CloudFront receives a viewer request, the
-- requested path is compared with path patterns in the order in which
-- cache behaviors are listed in the distribution.
--
-- You can optionally include a slash (@\/@) at the beginning of the path
-- pattern. For example, @\/images\/*.jpg@. CloudFront behavior is the same
-- with or without the leading @\/@.
--
-- The path pattern for the default cache behavior is @*@ and cannot be
-- changed. If the request for an object does not match the path pattern
-- for any cache behaviors, CloudFront applies the behavior in the default
-- cache behavior.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_pathPattern :: Lens.Lens' CacheBehavior Prelude.Text
cacheBehavior_pathPattern = Lens.lens (\CacheBehavior' {pathPattern} -> pathPattern) (\s@CacheBehavior' {} a -> s {pathPattern = a} :: CacheBehavior)

-- | The value of @ID@ for the origin that you want CloudFront to route
-- requests to when they match this cache behavior.
cacheBehavior_targetOriginId :: Lens.Lens' CacheBehavior Prelude.Text
cacheBehavior_targetOriginId = Lens.lens (\CacheBehavior' {targetOriginId} -> targetOriginId) (\s@CacheBehavior' {} a -> s {targetOriginId = a} :: CacheBehavior)

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
-- recommend that you clear your objects\' cache because cached objects are
-- protocol agnostic. That means that an edge location will return an
-- object from the cache regardless of whether the current request protocol
-- matches the protocol used previously. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing Cache Expiration>
-- in the /Amazon CloudFront Developer Guide/.
cacheBehavior_viewerProtocolPolicy :: Lens.Lens' CacheBehavior ViewerProtocolPolicy
cacheBehavior_viewerProtocolPolicy = Lens.lens (\CacheBehavior' {viewerProtocolPolicy} -> viewerProtocolPolicy) (\s@CacheBehavior' {} a -> s {viewerProtocolPolicy = a} :: CacheBehavior)

instance Data.FromXML CacheBehavior where
  parseXML x =
    CacheBehavior'
      Prelude.<$> (x Data..@? "AllowedMethods")
      Prelude.<*> (x Data..@? "CachePolicyId")
      Prelude.<*> (x Data..@? "Compress")
      Prelude.<*> (x Data..@? "DefaultTTL")
      Prelude.<*> (x Data..@? "FieldLevelEncryptionId")
      Prelude.<*> (x Data..@? "ForwardedValues")
      Prelude.<*> (x Data..@? "FunctionAssociations")
      Prelude.<*> (x Data..@? "LambdaFunctionAssociations")
      Prelude.<*> (x Data..@? "MaxTTL")
      Prelude.<*> (x Data..@? "MinTTL")
      Prelude.<*> (x Data..@? "OriginRequestPolicyId")
      Prelude.<*> (x Data..@? "RealtimeLogConfigArn")
      Prelude.<*> (x Data..@? "ResponseHeadersPolicyId")
      Prelude.<*> (x Data..@? "SmoothStreaming")
      Prelude.<*> (x Data..@? "TrustedKeyGroups")
      Prelude.<*> (x Data..@? "TrustedSigners")
      Prelude.<*> (x Data..@ "PathPattern")
      Prelude.<*> (x Data..@ "TargetOriginId")
      Prelude.<*> (x Data..@ "ViewerProtocolPolicy")

instance Prelude.Hashable CacheBehavior where
  hashWithSalt _salt CacheBehavior' {..} =
    _salt
      `Prelude.hashWithSalt` allowedMethods
      `Prelude.hashWithSalt` cachePolicyId
      `Prelude.hashWithSalt` compress
      `Prelude.hashWithSalt` defaultTTL
      `Prelude.hashWithSalt` fieldLevelEncryptionId
      `Prelude.hashWithSalt` forwardedValues
      `Prelude.hashWithSalt` functionAssociations
      `Prelude.hashWithSalt` lambdaFunctionAssociations
      `Prelude.hashWithSalt` maxTTL
      `Prelude.hashWithSalt` minTTL
      `Prelude.hashWithSalt` originRequestPolicyId
      `Prelude.hashWithSalt` realtimeLogConfigArn
      `Prelude.hashWithSalt` responseHeadersPolicyId
      `Prelude.hashWithSalt` smoothStreaming
      `Prelude.hashWithSalt` trustedKeyGroups
      `Prelude.hashWithSalt` trustedSigners
      `Prelude.hashWithSalt` pathPattern
      `Prelude.hashWithSalt` targetOriginId
      `Prelude.hashWithSalt` viewerProtocolPolicy

instance Prelude.NFData CacheBehavior where
  rnf CacheBehavior' {..} =
    Prelude.rnf allowedMethods `Prelude.seq`
      Prelude.rnf cachePolicyId `Prelude.seq`
        Prelude.rnf compress `Prelude.seq`
          Prelude.rnf defaultTTL `Prelude.seq`
            Prelude.rnf fieldLevelEncryptionId `Prelude.seq`
              Prelude.rnf forwardedValues `Prelude.seq`
                Prelude.rnf functionAssociations `Prelude.seq`
                  Prelude.rnf lambdaFunctionAssociations `Prelude.seq`
                    Prelude.rnf maxTTL `Prelude.seq`
                      Prelude.rnf minTTL `Prelude.seq`
                        Prelude.rnf originRequestPolicyId `Prelude.seq`
                          Prelude.rnf realtimeLogConfigArn `Prelude.seq`
                            Prelude.rnf responseHeadersPolicyId `Prelude.seq`
                              Prelude.rnf smoothStreaming `Prelude.seq`
                                Prelude.rnf trustedKeyGroups `Prelude.seq`
                                  Prelude.rnf trustedSigners `Prelude.seq`
                                    Prelude.rnf pathPattern `Prelude.seq`
                                      Prelude.rnf targetOriginId `Prelude.seq`
                                        Prelude.rnf viewerProtocolPolicy

instance Data.ToXML CacheBehavior where
  toXML CacheBehavior' {..} =
    Prelude.mconcat
      [ "AllowedMethods" Data.@= allowedMethods,
        "CachePolicyId" Data.@= cachePolicyId,
        "Compress" Data.@= compress,
        "DefaultTTL" Data.@= defaultTTL,
        "FieldLevelEncryptionId"
          Data.@= fieldLevelEncryptionId,
        "ForwardedValues" Data.@= forwardedValues,
        "FunctionAssociations" Data.@= functionAssociations,
        "LambdaFunctionAssociations"
          Data.@= lambdaFunctionAssociations,
        "MaxTTL" Data.@= maxTTL,
        "MinTTL" Data.@= minTTL,
        "OriginRequestPolicyId"
          Data.@= originRequestPolicyId,
        "RealtimeLogConfigArn" Data.@= realtimeLogConfigArn,
        "ResponseHeadersPolicyId"
          Data.@= responseHeadersPolicyId,
        "SmoothStreaming" Data.@= smoothStreaming,
        "TrustedKeyGroups" Data.@= trustedKeyGroups,
        "TrustedSigners" Data.@= trustedSigners,
        "PathPattern" Data.@= pathPattern,
        "TargetOriginId" Data.@= targetOriginId,
        "ViewerProtocolPolicy" Data.@= viewerProtocolPolicy
      ]
