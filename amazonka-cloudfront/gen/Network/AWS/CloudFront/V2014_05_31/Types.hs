{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudFront is a web service that speeds up distribution of your
-- static and dynamic web content, for example, .html, .css, .php, image, and
-- media files, to end users. CloudFront delivers your content through a
-- worldwide network of edge locations. When an end user requests content that
-- you're serving with CloudFront, the user is routed to the edge location
-- that provides the lowest latency, so content is delivered with the best
-- possible performance. If the content is already in that edge location,
-- CloudFront delivers it immediately. If the content is not currently in that
-- edge location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP
-- server (for example, a web server) that you have identified as the source
-- for the definitive version of your content.
module Network.AWS.CloudFront.V2014_05_31.Types
    (
    -- * Service
      CloudFront
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * GeoRestrictionType
    , GeoRestrictionType (..)

    -- * ItemSelection
    , ItemSelection (..)

    -- * Method
    , Method (..)

    -- * OriginProtocolPolicy
    , OriginProtocolPolicy (..)

    -- * PriceClass
    , PriceClass (..)

    -- * SSLSupportMethod
    , SSLSupportMethod (..)

    -- * ViewerProtocolPolicy
    , ViewerProtocolPolicy (..)

    -- * Restrictions
    , Restrictions (..)
    , rsGeoRestriction

    -- * S3OriginConfig
    , S3OriginConfig (..)
    , socOriginAccessIdentity

    -- * ActiveTrustedSigners
    , ActiveTrustedSigners (..)
    , atsEnabled
    , atsQuantity
    , atsItems

    -- * Aliases
    , Aliases (..)
    , aQuantity
    , aItems

    -- * AllowedMethods
    , AllowedMethods (..)
    , amQuantity
    , amItems

    -- * CacheBehavior
    , CacheBehavior (..)
    , ccPathPattern
    , ccTargetOriginId
    , ccForwardedValues
    , ccTrustedSigners
    , ccViewerProtocolPolicy
    , ccMinTTL
    , ccAllowedMethods
    , ccSmoothStreaming

    -- * CacheBehaviors
    , CacheBehaviors (..)
    , cbQuantity
    , cbItems

    -- * CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity (..)
    , cfoaiId
    , cfoaiS3CanonicalUserId
    , cfoaiCloudFrontOriginAccessIdentityConfig

    -- * CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig (..)
    , cfoaicCallerReference
    , cfoaicComment

    -- * CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList (..)
    , cfoailMarker
    , cfoailNextMarker
    , cfoailMaxItems
    , cfoailIsTruncated
    , cfoailQuantity
    , cfoailItems

    -- * CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary (..)
    , cfoaisId
    , cfoaisS3CanonicalUserId
    , cfoaisComment

    -- * CookieNames
    , CookieNames (..)
    , cnQuantity
    , cnItems

    -- * CookiePreference
    , CookiePreference (..)
    , cpForward
    , cpWhitelistedNames

    -- * CustomErrorResponse
    , CustomErrorResponse (..)
    , cesErrorCode
    , cesResponsePagePath
    , cesResponseCode
    , cesErrorCachingMinTTL

    -- * CustomErrorResponses
    , CustomErrorResponses (..)
    , cerQuantity
    , cerItems

    -- * CustomOriginConfig
    , CustomOriginConfig (..)
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- * DefaultCacheBehavior
    , DefaultCacheBehavior (..)
    , dcbTargetOriginId
    , dcbForwardedValues
    , dcbTrustedSigners
    , dcbViewerProtocolPolicy
    , dcbMinTTL
    , dcbAllowedMethods
    , dcbSmoothStreaming

    -- * Distribution
    , Distribution (..)
    , dnId
    , dnStatus
    , dnLastModifiedTime
    , dnInProgressInvalidationBatches
    , dnDomainName
    , dnActiveTrustedSigners
    , dnDistributionConfig

    -- * DistributionConfig
    , DistributionConfig (..)
    , dcCallerReference
    , dcAliases
    , dcDefaultRootObject
    , dcOrigins
    , dcDefaultCacheBehavior
    , dcCacheBehaviors
    , dcCustomErrorResponses
    , dcComment
    , dcLogging
    , dcPriceClass
    , dcEnabled
    , dcViewerCertificate
    , dcRestrictions

    -- * DistributionList
    , DistributionList (..)
    , dlMarker
    , dlNextMarker
    , dlMaxItems
    , dlIsTruncated
    , dlQuantity
    , dlItems

    -- * DistributionSummary
    , DistributionSummary (..)
    , dsId
    , dsStatus
    , dsLastModifiedTime
    , dsDomainName
    , dsAliases
    , dsOrigins
    , dsDefaultCacheBehavior
    , dsCacheBehaviors
    , dsCustomErrorResponses
    , dsComment
    , dsPriceClass
    , dsEnabled
    , dsViewerCertificate
    , dsRestrictions

    -- * ForwardedValues
    , ForwardedValues (..)
    , fvQueryString
    , fvCookies
    , fvHeaders

    -- * GeoRestriction
    , GeoRestriction (..)
    , grRestrictionType
    , grQuantity
    , grItems

    -- * Headers
    , Headers (..)
    , hQuantity
    , hItems

    -- * Invalidation
    , Invalidation (..)
    , inId
    , inStatus
    , inCreateTime
    , inInvalidationBatch

    -- * InvalidationBatch
    , InvalidationBatch (..)
    , ibPaths
    , ibCallerReference

    -- * InvalidationList
    , InvalidationList (..)
    , ilMarker
    , ilNextMarker
    , ilMaxItems
    , ilIsTruncated
    , ilQuantity
    , ilItems

    -- * InvalidationSummary
    , InvalidationSummary (..)
    , iiiiiiiiiiiiyId
    , iiiiiiiiiiiiyCreateTime
    , iiiiiiiiiiiiyStatus

    -- * KeyPairIds
    , KeyPairIds (..)
    , kpiQuantity
    , kpiItems

    -- * LoggingConfig
    , LoggingConfig (..)
    , lcEnabled
    , lcIncludeCookies
    , lcBucket
    , lcPrefix

    -- * Origin
    , Origin (..)
    , pId
    , pDomainName
    , pS3OriginConfig
    , pCustomOriginConfig

    -- * Origins
    , Origins (..)
    , oQuantity
    , oItems

    -- * Paths
    , Paths (..)
    , psQuantity
    , psItems

    -- * S3Origin
    , S3Origin (..)
    , ssssnDomainName
    , ssssnOriginAccessIdentity

    -- * Signer
    , Signer (..)
    , ssrAwsAccountNumber
    , ssrKeyPairIds

    -- * StreamingDistribution
    , StreamingDistribution (..)
    , sdId
    , sdStatus
    , sdLastModifiedTime
    , sdDomainName
    , sdActiveTrustedSigners
    , sdStreamingDistributionConfig

    -- * StreamingDistributionConfig
    , StreamingDistributionConfig (..)
    , sdcCallerReference
    , sdcS3Origin
    , sdcAliases
    , sdcComment
    , sdcLogging
    , sdcTrustedSigners
    , sdcPriceClass
    , sdcEnabled

    -- * StreamingDistributionList
    , StreamingDistributionList (..)
    , sdlMarker
    , sdlNextMarker
    , sdlMaxItems
    , sdlIsTruncated
    , sdlQuantity
    , sdlItems

    -- * StreamingDistributionSummary
    , StreamingDistributionSummary (..)
    , sdsId
    , sdsStatus
    , sdsLastModifiedTime
    , sdsDomainName
    , sdsS3Origin
    , sdsAliases
    , sdsTrustedSigners
    , sdsComment
    , sdsPriceClass
    , sdsEnabled

    -- * StreamingLoggingConfig
    , StreamingLoggingConfig (..)
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- * TrustedSigners
    , TrustedSigners (..)
    , tsEnabled
    , tsQuantity
    , tsItems

    -- * ViewerCertificate
    , ViewerCertificate (..)
    , vcIAMCertificateId
    , vcCloudFrontDefaultCertificate
    , vcSSLSupportMethod

    ) where

import           Network.AWS.Prelude
import           Network.AWS.Signing.V4
import           Network.AWS.Types     (Region)
import qualified Network.AWS.Types.Map as Map

-- | Supported version (@2014-05-31@) of the
-- @Amazon CloudFront@ service.
data CloudFront deriving (Typeable)

instance AWSService CloudFront where
    type Sg CloudFront = V4
    data Er CloudFront
        = AccessDenied
            { _adMessage :: Maybe Text
            }
        | BatchTooLarge
            { _btlMessage :: Maybe Text
            }
        | CNAMEAlreadyExists
            { _cnameaeMessage :: Maybe Text
            }
        | CloudFrontClient HttpException
        | CloudFrontOriginAccessIdentityAlreadyExists
            { _cfoaiaeMessage :: Maybe Text
            }
        | CloudFrontOriginAccessIdentityInUse
            { _cfoaiiuMessage :: Maybe Text
            }
        | CloudFrontSerializer String
        | CloudFrontService String
        | DistributionAlreadyExists
            { _daeMessage :: Maybe Text
            }
        | DistributionNotDisabled
            { _dndMessage :: Maybe Text
            }
        | IllegalUpdate
            { _iuMessage :: Maybe Text
            }
        | InconsistentQuantities
            { _iqMessage :: Maybe Text
            }
        | InvalidArgument
            { _iaMessage :: Maybe Text
            }
        | InvalidDefaultRootObject
            { _idroMessage :: Maybe Text
            }
        | InvalidErrorCode
            { _iecMessage :: Maybe Text
            }
        | InvalidForwardCookies
            { _ifcMessage :: Maybe Text
            }
        | InvalidGeoRestrictionParameter
            { _igrpMessage :: Maybe Text
            }
        | InvalidHeadersForS3Origin
            { _ihfsoMessage :: Maybe Text
            }
        | InvalidIfMatchVersion
            { _iimvMessage :: Maybe Text
            }
        | InvalidLocationCode
            { _ilcMessage :: Maybe Text
            }
        | InvalidOrigin
            { _ioMessage :: Maybe Text
            }
        | InvalidOriginAccessIdentity
            { _ioaiMessage :: Maybe Text
            }
        | InvalidRelativePath
            { _irpMessage :: Maybe Text
            }
        | InvalidRequiredProtocol
            { _irpMessage :: Maybe Text
            }
        | InvalidResponseCode
            { _ircMessage :: Maybe Text
            }
        | InvalidViewerCertificate
            { _ivcMessage :: Maybe Text
            }
        | MissingBody
            { _mbMessage :: Maybe Text
            }
        | NoSuchCloudFrontOriginAccessIdentity
            { _nscfoaiMessage :: Maybe Text
            }
        | NoSuchDistribution
            { _nsdMessage :: Maybe Text
            }
        | NoSuchInvalidation
            { _nsiMessage :: Maybe Text
            }
        | NoSuchOrigin
            { _nsoMessage :: Maybe Text
            }
        | NoSuchStreamingDistribution
            { _nssdMessage :: Maybe Text
            }
        | PreconditionFailed
            { _pfMessage :: Maybe Text
            }
        | StreamingDistributionAlreadyExists
            { _sdaeMessage :: Maybe Text
            }
        | StreamingDistributionNotDisabled
            { _sdndMessage :: Maybe Text
            }
        | TooManyCacheBehaviors
            { _tmcbMessage :: Maybe Text
            }
        | TooManyCertificates
            { _tmcMessage :: Maybe Text
            }
        | TooManyCloudFrontOriginAccessIdentities
            { _tmcfoaiMessage :: Maybe Text
            }
        | TooManyCookieNamesInWhiteList
            { _tmcniwlMessage :: Maybe Text
            }
        | TooManyDistributionCNAMEs
            { _tmdcnameMessage :: Maybe Text
            }
        | TooManyDistributions
            { _tmdMessage :: Maybe Text
            }
        | TooManyHeadersInForwardedValues
            { _tmhifvMessage :: Maybe Text
            }
        | TooManyInvalidationsInProgress
            { _tmiipMessage :: Maybe Text
            }
        | TooManyOrigins
            { _tmoMessage :: Maybe Text
            }
        | TooManyStreamingDistributionCNAMEs
            { _tmsdcnameMessage :: Maybe Text
            }
        | TooManyStreamingDistributions
            { _tmsdMessage :: Maybe Text
            }
        | TooManyTrustedSigners
            { _tmtsMessage :: Maybe Text
            }
        | TrustedSignerDoesNotExist
            { _tsdneMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudfront"
        , _svcVersion  = "2014-05-31"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudFront)
deriving instance Generic (Er CloudFront)

instance AWSError (Er CloudFront) where
    awsError = const "CloudFrontError"

instance AWSServiceError (Er CloudFront) where
    serviceError    = CloudFrontService
    clientError     = CloudFrontClient
    serializerError = CloudFrontSerializer

instance Exception (Er CloudFront)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://cloudfront.amazonaws.com/doc/2014-05-31/"
    }

-- | The method that you want to use to restrict distribution of your content by
-- country: - none: No geo restriction is enabled, meaning access to content
-- is not restricted by client geo location. - blacklist: The Location
-- elements specify the countries in which you do not want CloudFront to
-- distribute your content. - whitelist: The Location elements specify the
-- countries in which you want CloudFront to distribute your content.
data GeoRestrictionType
    = GeoRestrictionTypeBlacklist -- ^ blacklist
    | GeoRestrictionTypeNone -- ^ none
    | GeoRestrictionTypeWhitelist -- ^ whitelist
      deriving (Eq, Show, Generic)

instance Hashable GeoRestrictionType

instance FromText GeoRestrictionType where
    parser = match "blacklist" GeoRestrictionTypeBlacklist
         <|> match "none" GeoRestrictionTypeNone
         <|> match "whitelist" GeoRestrictionTypeWhitelist

instance ToText GeoRestrictionType where
    toText GeoRestrictionTypeBlacklist = "blacklist"
    toText GeoRestrictionTypeNone = "none"
    toText GeoRestrictionTypeWhitelist = "whitelist"

instance ToByteString GeoRestrictionType

instance FromXML GeoRestrictionType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GeoRestrictionType"

instance ToQuery GeoRestrictionType where
      toQuery = toQuery . toBS

-- | Use this element to specify whether you want CloudFront to forward cookies
-- to the origin that is associated with this cache behavior. You can specify
-- all, none or whitelist. If you choose All, CloudFront forwards all cookies
-- regardless of how many your application uses.
data ItemSelection
    = ItemSelectionAll -- ^ all
    | ItemSelectionNone -- ^ none
    | ItemSelectionWhitelist -- ^ whitelist
      deriving (Eq, Show, Generic)

instance Hashable ItemSelection

instance FromText ItemSelection where
    parser = match "all" ItemSelectionAll
         <|> match "none" ItemSelectionNone
         <|> match "whitelist" ItemSelectionWhitelist

instance ToText ItemSelection where
    toText ItemSelectionAll = "all"
    toText ItemSelectionNone = "none"
    toText ItemSelectionWhitelist = "whitelist"

instance ToByteString ItemSelection

instance FromXML ItemSelection where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ItemSelection"

instance ToXML ItemSelection where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ItemSelection"

instance ToQuery ItemSelection where
      toQuery = toQuery . toBS

data Method
    = MethodDelete -- ^ DELETE
    | MethodGet -- ^ GET
    | MethodHead -- ^ HEAD
    | MethodOptions -- ^ OPTIONS
    | MethodPatch -- ^ PATCH
    | MethodPost -- ^ POST
    | MethodPut -- ^ PUT
      deriving (Eq, Show, Generic)

instance Hashable Method

instance FromText Method where
    parser = match "DELETE" MethodDelete
         <|> match "GET" MethodGet
         <|> match "HEAD" MethodHead
         <|> match "OPTIONS" MethodOptions
         <|> match "PATCH" MethodPatch
         <|> match "POST" MethodPost
         <|> match "PUT" MethodPut

instance ToText Method where
    toText MethodDelete = "DELETE"
    toText MethodGet = "GET"
    toText MethodHead = "HEAD"
    toText MethodOptions = "OPTIONS"
    toText MethodPatch = "PATCH"
    toText MethodPost = "POST"
    toText MethodPut = "PUT"

instance ToByteString Method

instance FromXML Method where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Method"

instance ToXML Method where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Method"

instance ToQuery Method where
      toQuery = toQuery . toBS

-- | The origin protocol policy to apply to your origin.
data OriginProtocolPolicy
    = OriginProtocolPolicyHttpOnly -- ^ http-only
    | OriginProtocolPolicyMatchViewer -- ^ match-viewer
      deriving (Eq, Show, Generic)

instance Hashable OriginProtocolPolicy

instance FromText OriginProtocolPolicy where
    parser = match "http-only" OriginProtocolPolicyHttpOnly
         <|> match "match-viewer" OriginProtocolPolicyMatchViewer

instance ToText OriginProtocolPolicy where
    toText OriginProtocolPolicyHttpOnly = "http-only"
    toText OriginProtocolPolicyMatchViewer = "match-viewer"

instance ToByteString OriginProtocolPolicy

instance FromXML OriginProtocolPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OriginProtocolPolicy"

instance ToXML OriginProtocolPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "OriginProtocolPolicy"

instance ToQuery OriginProtocolPolicy where
      toQuery = toQuery . toBS

-- | A complex type that contains information about price class for this
-- distribution.
data PriceClass
    = PriceClassPriceclass100 -- ^ PriceClass_100
    | PriceClassPriceclass200 -- ^ PriceClass_200
    | PriceClassPriceclassAll -- ^ PriceClass_All
      deriving (Eq, Show, Generic)

instance Hashable PriceClass

instance FromText PriceClass where
    parser = match "PriceClass_100" PriceClassPriceclass100
         <|> match "PriceClass_200" PriceClassPriceclass200
         <|> match "PriceClass_All" PriceClassPriceclassAll

instance ToText PriceClass where
    toText PriceClassPriceclass100 = "PriceClass_100"
    toText PriceClassPriceclass200 = "PriceClass_200"
    toText PriceClassPriceclassAll = "PriceClass_All"

instance ToByteString PriceClass

instance FromXML PriceClass where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PriceClass"

instance ToXML PriceClass where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "PriceClass"

instance ToQuery PriceClass where
      toQuery = toQuery . toBS

-- | If you specify a value for IAMCertificateId, you must also specify how you
-- want CloudFront to serve HTTPS requests. Valid values are vip and sni-only.
-- If you specify vip, CloudFront uses dedicated IP addresses for your content
-- and can respond to HTTPS requests from any viewer. However, you must
-- request permission to use this feature, and you incur additional monthly
-- charges. If you specify sni-only, CloudFront can only respond to HTTPS
-- requests from viewers that support Server Name Indication (SNI). All modern
-- browsers support SNI, but some browsers still in use don't support SNI. Do
-- not specify a value for SSLSupportMethod if you specified true for
-- CloudFrontDefaultCertificate.
data SSLSupportMethod
    = SSLSupportMethodSniOnly -- ^ sni-only
    | SSLSupportMethodVip -- ^ vip
      deriving (Eq, Show, Generic)

instance Hashable SSLSupportMethod

instance FromText SSLSupportMethod where
    parser = match "sni-only" SSLSupportMethodSniOnly
         <|> match "vip" SSLSupportMethodVip

instance ToText SSLSupportMethod where
    toText SSLSupportMethodSniOnly = "sni-only"
    toText SSLSupportMethodVip = "vip"

instance ToByteString SSLSupportMethod

instance FromXML SSLSupportMethod where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SSLSupportMethod"

instance ToXML SSLSupportMethod where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "SSLSupportMethod"

instance ToQuery SSLSupportMethod where
      toQuery = toQuery . toBS

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to
-- use any available protocol, specify allow-all. If you want CloudFront to
-- require HTTPS, specify https. If you want CloudFront to respond to an HTTP
-- request with an HTTP status code of 301 (Moved Permanently) and the HTTPS
-- URL, specify redirect-to-https. The viewer then resubmits the request using
-- the HTTPS URL.
data ViewerProtocolPolicy
    = ViewerProtocolPolicyAllowAll -- ^ allow-all
    | ViewerProtocolPolicyHttpsOnly -- ^ https-only
    | ViewerProtocolPolicyRedirectToHttps -- ^ redirect-to-https
      deriving (Eq, Show, Generic)

instance Hashable ViewerProtocolPolicy

instance FromText ViewerProtocolPolicy where
    parser = match "allow-all" ViewerProtocolPolicyAllowAll
         <|> match "https-only" ViewerProtocolPolicyHttpsOnly
         <|> match "redirect-to-https" ViewerProtocolPolicyRedirectToHttps

instance ToText ViewerProtocolPolicy where
    toText ViewerProtocolPolicyAllowAll = "allow-all"
    toText ViewerProtocolPolicyHttpsOnly = "https-only"
    toText ViewerProtocolPolicyRedirectToHttps = "redirect-to-https"

instance ToByteString ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ViewerProtocolPolicy"

instance ToQuery ViewerProtocolPolicy where
      toQuery = toQuery . toBS

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
newtype Restrictions = Restrictions
    { _rsGeoRestriction :: GeoRestriction
      -- ^ A complex type that controls the countries in which your content
      -- is distributed. For more information about geo restriction, go to
      -- Customizing Error Responses in the Amazon CloudFront Developer
      -- Guide. CloudFront determines the location of your users using
      -- MaxMind GeoIP databases. For information about the accuracy of
      -- these databases, see How accurate are your GeoIP databases? on
      -- the MaxMind website.
    } deriving (Show, Generic)

-- | A complex type that controls the countries in which your content is
-- distributed. For more information about geo restriction, go to Customizing
-- Error Responses in the Amazon CloudFront Developer Guide. CloudFront
-- determines the location of your users using MaxMind GeoIP databases. For
-- information about the accuracy of these databases, see How accurate are
-- your GeoIP databases? on the MaxMind website.
rsGeoRestriction :: Lens' Restrictions (GeoRestriction)
rsGeoRestriction f x =
    f (_rsGeoRestriction x)
        <&> \y -> x { _rsGeoRestriction = y }
{-# INLINE rsGeoRestriction #-}

instance FromXML Restrictions where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Restrictions"

instance ToXML Restrictions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Restrictions"

-- | A complex type that contains information about the Amazon S3 origin. If the
-- origin is a custom origin, use the CustomOriginConfig element instead.
newtype S3OriginConfig = S3OriginConfig
    { _socOriginAccessIdentity :: Text
      -- ^ The CloudFront origin access identity to associate with the
      -- origin. Use an origin access identity to configure the origin so
      -- that end users can only access objects in an Amazon S3 bucket
      -- through CloudFront. If you want end users to be able to access
      -- objects using either the CloudFront URL or the Amazon S3 URL,
      -- specify an empty OriginAccessIdentity element. To delete the
      -- origin access identity from an existing distribution, update the
      -- distribution configuration and include an empty
      -- OriginAccessIdentity element. To replace the origin access
      -- identity, update the distribution configuration and specify the
      -- new origin access identity. Use the format
      -- origin-access-identity/cloudfront/Id where Id is the value that
      -- CloudFront returned in the Id element when you created the origin
      -- access identity.
    } deriving (Show, Generic)

-- | The CloudFront origin access identity to associate with the origin. Use an
-- origin access identity to configure the origin so that end users can only
-- access objects in an Amazon S3 bucket through CloudFront. If you want end
-- users to be able to access objects using either the CloudFront URL or the
-- Amazon S3 URL, specify an empty OriginAccessIdentity element. To delete the
-- origin access identity from an existing distribution, update the
-- distribution configuration and include an empty OriginAccessIdentity
-- element. To replace the origin access identity, update the distribution
-- configuration and specify the new origin access identity. Use the format
-- origin-access-identity/cloudfront/Id where Id is the value that CloudFront
-- returned in the Id element when you created the origin access identity.
socOriginAccessIdentity :: Lens' S3OriginConfig (Text)
socOriginAccessIdentity f x =
    f (_socOriginAccessIdentity x)
        <&> \y -> x { _socOriginAccessIdentity = y }
{-# INLINE socOriginAccessIdentity #-}

instance FromXML S3OriginConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3OriginConfig"

instance ToXML S3OriginConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "S3OriginConfig"

-- | CloudFront automatically adds this element to the response only if you've
-- set up the distribution to serve private content with signed URLs. The
-- element lists the key pair IDs that CloudFront is aware of for each trusted
-- signer. The Signer child element lists the AWS account number of the
-- trusted signer (or an empty Self element if the signer is you). The Signer
-- element also includes the IDs of any active key pairs associated with the
-- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
-- that signer can't create working signed URLs.
data ActiveTrustedSigners = ActiveTrustedSigners
    { _atsEnabled :: Bool
      -- ^ Each active trusted signer.
    , _atsQuantity :: Integer
      -- ^ The number of unique trusted signers included in all cache
      -- behaviors. For example, if three cache behaviors all list the
      -- same three AWS accounts, the value of Quantity for
      -- ActiveTrustedSigners will be 3.
    , _atsItems :: [Signer]
      -- ^ A complex type that contains one Signer complex type for each
      -- unique trusted signer that is specified in the TrustedSigners
      -- complex type, including trusted signers in the default cache
      -- behavior and in all of the other cache behaviors.
    } deriving (Show, Generic)

-- | Each active trusted signer.
atsEnabled :: Lens' ActiveTrustedSigners (Bool)
atsEnabled f x =
    f (_atsEnabled x)
        <&> \y -> x { _atsEnabled = y }
{-# INLINE atsEnabled #-}

-- | The number of unique trusted signers included in all cache behaviors. For
-- example, if three cache behaviors all list the same three AWS accounts, the
-- value of Quantity for ActiveTrustedSigners will be 3.
atsQuantity :: Lens' ActiveTrustedSigners (Integer)
atsQuantity f x =
    f (_atsQuantity x)
        <&> \y -> x { _atsQuantity = y }
{-# INLINE atsQuantity #-}

-- | A complex type that contains one Signer complex type for each unique
-- trusted signer that is specified in the TrustedSigners complex type,
-- including trusted signers in the default cache behavior and in all of the
-- other cache behaviors.
atsItems :: Lens' ActiveTrustedSigners ([Signer])
atsItems f x =
    f (_atsItems x)
        <&> \y -> x { _atsItems = y }
{-# INLINE atsItems #-}

instance FromXML ActiveTrustedSigners where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ActiveTrustedSigners"

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
data Aliases = Aliases
    { _aQuantity :: Integer
      -- ^ The number of CNAMEs, if any, for this distribution.
    , _aItems :: [Text]
      -- ^ Optional: A complex type that contains CNAME elements, if any,
      -- for this distribution. If Quantity is 0, you can omit Items.
    } deriving (Show, Generic)

-- | The number of CNAMEs, if any, for this distribution.
aQuantity :: Lens' Aliases (Integer)
aQuantity f x =
    f (_aQuantity x)
        <&> \y -> x { _aQuantity = y }
{-# INLINE aQuantity #-}

-- | Optional: A complex type that contains CNAME elements, if any, for this
-- distribution. If Quantity is 0, you can omit Items.
aItems :: Lens' Aliases ([Text])
aItems f x =
    f (_aItems x)
        <&> \y -> x { _aItems = y }
{-# INLINE aItems #-}

instance FromXML Aliases where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Aliases"

instance ToXML Aliases where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Aliases"

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are two
-- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
-- choose the second option, you may need to restrict access to your Amazon S3
-- bucket or to your custom origin so users can't perform operations that you
-- don't want them to. For example, you may not want users to have permission
-- to delete objects from your origin.
data AllowedMethods = AllowedMethods
    { _amQuantity :: Integer
      -- ^ The number of HTTP methods that you want CloudFront to forward to
      -- your origin. Valid values are 2 (for GET and HEAD requests) and 7
      -- (for DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests).
    , _amItems :: [Method]
      -- ^ A complex type that contains the HTTP methods that you want
      -- CloudFront to process and forward to your origin.
    } deriving (Show, Generic)

-- | The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for GET and HEAD requests) and 7 (for DELETE,
-- GET, HEAD, OPTIONS, PATCH, POST, and PUT requests).
amQuantity :: Lens' AllowedMethods (Integer)
amQuantity f x =
    f (_amQuantity x)
        <&> \y -> x { _amQuantity = y }
{-# INLINE amQuantity #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to
-- process and forward to your origin.
amItems :: Lens' AllowedMethods ([Method])
amItems f x =
    f (_amItems x)
        <&> \y -> x { _amItems = y }
{-# INLINE amItems #-}

instance FromXML AllowedMethods where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AllowedMethods"

instance ToXML AllowedMethods where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "AllowedMethods"

-- | A complex type that describes how CloudFront processes requests. You can
-- create up to 10 cache behaviors.You must create at least as many cache
-- behaviors (including the default cache behavior) as you have origins if you
-- want CloudFront to distribute objects from all of the origins. Each cache
-- behavior specifies the one origin from which you want CloudFront to get
-- objects. If you have two origins and only the default cache behavior, the
-- default cache behavior will cause CloudFront to get objects from one of the
-- origins, but the other origin will never be used. If you don't want to
-- specify any cache behaviors, include only an empty CacheBehaviors element.
-- Don't include an empty CacheBehavior element, or CloudFront returns a
-- MalformedXML error. To delete all cache behaviors in an existing
-- distribution, update the distribution configuration and include only an
-- empty CacheBehaviors element. To add, change, or remove one or more cache
-- behaviors, update the distribution configuration and specify all of the
-- cache behaviors that you want to include in the updated distribution.
data CacheBehavior = CacheBehavior
    { _ccPathPattern :: Text
      -- ^ The pattern (for example, images/*.jpg) that specifies which
      -- requests you want this cache behavior to apply to. When
      -- CloudFront receives an end-user request, the requested path is
      -- compared with path patterns in the order in which cache behaviors
      -- are listed in the distribution. The path pattern for the default
      -- cache behavior is * and cannot be changed. If the request for an
      -- object does not match the path pattern for any cache behaviors,
      -- CloudFront applies the behavior in the default cache behavior.
    , _ccTargetOriginId :: Text
      -- ^ The value of ID for the origin that you want CloudFront to route
      -- requests to when a request matches the path pattern either for a
      -- cache behavior or for the default cache behavior.
    , _ccForwardedValues :: ForwardedValues
      -- ^ A complex type that specifies how CloudFront handles query
      -- strings, cookies and headers.
    , _ccTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you
      -- want to allow to create signed URLs for private content. If you
      -- want to require signed URLs in requests for objects in the target
      -- origin that match the PathPattern for this cache behavior,
      -- specify true for Enabled, and specify the applicable values for
      -- Quantity and Items. For more information, go to Using a Signed
      -- URL to Serve Private Content in the Amazon CloudFront Developer
      -- Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0
      -- for Quantity. Omit Items. To add, change, or remove one or more
      -- trusted signers, change Enabled to true (if it's currently
      -- false), change Quantity as applicable, and specify all of the
      -- trusted signers that you want to include in the updated
      -- distribution.
    , _ccViewerProtocolPolicy :: ViewerProtocolPolicy
      -- ^ Use this element to specify the protocol that users can use to
      -- access the files in the origin specified by TargetOriginId when a
      -- request matches the path pattern in PathPattern. If you want
      -- CloudFront to allow end users to use any available protocol,
      -- specify allow-all. If you want CloudFront to require HTTPS,
      -- specify https. If you want CloudFront to respond to an HTTP
      -- request with an HTTP status code of 301 (Moved Permanently) and
      -- the HTTPS URL, specify redirect-to-https. The viewer then
      -- resubmits the request using the HTTPS URL.
    , _ccMinTTL :: Integer
      -- ^ The minimum amount of time that you want objects to stay in
      -- CloudFront caches before CloudFront queries your origin to see
      -- whether the object has been updated.You can specify a value from
      -- 0 to 3,153,600,000 seconds (100 years).
    , _ccAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront
      -- processes and forwards to your Amazon S3 bucket or your custom
      -- origin. There are two options: - CloudFront forwards only GET and
      -- HEAD requests. - CloudFront forwards DELETE, GET, HEAD, OPTIONS,
      -- PATCH, POST, and PUT requests. If you choose the second option,
      -- you may need to restrict access to your Amazon S3 bucket or to
      -- your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have
      -- permission to delete objects from your origin.
    , _ccSmoothStreaming :: Maybe Bool
      -- ^ Indicates whether you want to distribute media files in Microsoft
      -- Smooth Streaming format using the origin that is associated with
      -- this cache behavior. If so, specify true; if not, specify false.
    } deriving (Show, Generic)

-- | The pattern (for example, images/*.jpg) that specifies which requests you
-- want this cache behavior to apply to. When CloudFront receives an end-user
-- request, the requested path is compared with path patterns in the order in
-- which cache behaviors are listed in the distribution. The path pattern for
-- the default cache behavior is * and cannot be changed. If the request for
-- an object does not match the path pattern for any cache behaviors,
-- CloudFront applies the behavior in the default cache behavior.
ccPathPattern :: Lens' CacheBehavior (Text)
ccPathPattern f x =
    f (_ccPathPattern x)
        <&> \y -> x { _ccPathPattern = y }
{-# INLINE ccPathPattern #-}

-- | The value of ID for the origin that you want CloudFront to route requests
-- to when a request matches the path pattern either for a cache behavior or
-- for the default cache behavior.
ccTargetOriginId :: Lens' CacheBehavior (Text)
ccTargetOriginId f x =
    f (_ccTargetOriginId x)
        <&> \y -> x { _ccTargetOriginId = y }
{-# INLINE ccTargetOriginId #-}

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
ccForwardedValues :: Lens' CacheBehavior (ForwardedValues)
ccForwardedValues f x =
    f (_ccForwardedValues x)
        <&> \y -> x { _ccForwardedValues = y }
{-# INLINE ccForwardedValues #-}

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
ccTrustedSigners :: Lens' CacheBehavior (TrustedSigners)
ccTrustedSigners f x =
    f (_ccTrustedSigners x)
        <&> \y -> x { _ccTrustedSigners = y }
{-# INLINE ccTrustedSigners #-}

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to
-- use any available protocol, specify allow-all. If you want CloudFront to
-- require HTTPS, specify https. If you want CloudFront to respond to an HTTP
-- request with an HTTP status code of 301 (Moved Permanently) and the HTTPS
-- URL, specify redirect-to-https. The viewer then resubmits the request using
-- the HTTPS URL.
ccViewerProtocolPolicy :: Lens' CacheBehavior (ViewerProtocolPolicy)
ccViewerProtocolPolicy f x =
    f (_ccViewerProtocolPolicy x)
        <&> \y -> x { _ccViewerProtocolPolicy = y }
{-# INLINE ccViewerProtocolPolicy #-}

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
-- years).
ccMinTTL :: Lens' CacheBehavior (Integer)
ccMinTTL f x =
    f (_ccMinTTL x)
        <&> \y -> x { _ccMinTTL = y }
{-# INLINE ccMinTTL #-}

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are two
-- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
-- choose the second option, you may need to restrict access to your Amazon S3
-- bucket or to your custom origin so users can't perform operations that you
-- don't want them to. For example, you may not want users to have permission
-- to delete objects from your origin.
ccAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
ccAllowedMethods f x =
    f (_ccAllowedMethods x)
        <&> \y -> x { _ccAllowedMethods = y }
{-# INLINE ccAllowedMethods #-}

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
ccSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
ccSmoothStreaming f x =
    f (_ccSmoothStreaming x)
        <&> \y -> x { _ccSmoothStreaming = y }
{-# INLINE ccSmoothStreaming #-}

instance FromXML CacheBehavior where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheBehavior"

instance ToXML CacheBehavior where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CacheBehavior"

-- | A complex type that contains zero or more CacheBehavior elements.
data CacheBehaviors = CacheBehaviors
    { _cbQuantity :: Integer
      -- ^ The number of cache behaviors for this distribution.
    , _cbItems :: [CacheBehavior]
      -- ^ Optional: A complex type that contains cache behaviors for this
      -- distribution. If Quantity is 0, you can omit Items.
    } deriving (Show, Generic)

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors (Integer)
cbQuantity f x =
    f (_cbQuantity x)
        <&> \y -> x { _cbQuantity = y }
{-# INLINE cbQuantity #-}

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If Quantity is 0, you can omit Items.
cbItems :: Lens' CacheBehaviors ([CacheBehavior])
cbItems f x =
    f (_cbItems x)
        <&> \y -> x { _cbItems = y }
{-# INLINE cbItems #-}

instance FromXML CacheBehaviors where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheBehaviors"

instance ToXML CacheBehaviors where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CacheBehaviors"

-- | The origin access identity's information.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity
    { _cfoaiId :: Text
      -- ^ The ID for the origin access identity. For example:
      -- E74FTE3AJFJ256A.
    , _cfoaiS3CanonicalUserId :: Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity,
      -- which you use when giving the origin access identity read
      -- permission to an object in Amazon S3.
    , _cfoaiCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
      -- ^ The current configuration information for the identity.
    } deriving (Show, Generic)

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaiId :: Lens' CloudFrontOriginAccessIdentity (Text)
cfoaiId f x =
    f (_cfoaiId x)
        <&> \y -> x { _cfoaiId = y }
{-# INLINE cfoaiId #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you
-- use when giving the origin access identity read permission to an object in
-- Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity (Text)
cfoaiS3CanonicalUserId f x =
    f (_cfoaiS3CanonicalUserId x)
        <&> \y -> x { _cfoaiS3CanonicalUserId = y }
{-# INLINE cfoaiS3CanonicalUserId #-}

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig f x =
    f (_cfoaiCloudFrontOriginAccessIdentityConfig x)
        <&> \y -> x { _cfoaiCloudFrontOriginAccessIdentityConfig = y }
{-# INLINE cfoaiCloudFrontOriginAccessIdentityConfig #-}

instance FromXML CloudFrontOriginAccessIdentity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentity"

-- | The origin access identity's configuration information.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig
    { _cfoaicCallerReference :: Text
      -- ^ A unique number that ensures the request can't be replayed. If
      -- the CallerReference is new (no matter the content of the
      -- CloudFrontOriginAccessIdentityConfig object), a new origin access
      -- identity is created. If the CallerReference is a value you
      -- already sent in a previous request to create an identity, and the
      -- content of the CloudFrontOriginAccessIdentityConfig is identical
      -- to the original request (ignoring white space), the response
      -- includes the same information returned to the original request.
      -- If the CallerReference is a value you already sent in a previous
      -- request to create an identity but the content of the
      -- CloudFrontOriginAccessIdentityConfig is different from the
      -- original request, CloudFront returns a
      -- CloudFrontOriginAccessIdentityAlreadyExists error.
    , _cfoaicComment :: Text
      -- ^ Any comments you want to include about the origin access
      -- identity.
    } deriving (Show, Generic)

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the
-- CloudFrontOriginAccessIdentityConfig object), a new origin access identity
-- is created. If the CallerReference is a value you already sent in a
-- previous request to create an identity, and the content of the
-- CloudFrontOriginAccessIdentityConfig is identical to the original request
-- (ignoring white space), the response includes the same information returned
-- to the original request. If the CallerReference is a value you already sent
-- in a previous request to create an identity but the content of the
-- CloudFrontOriginAccessIdentityConfig is different from the original
-- request, CloudFront returns a CloudFrontOriginAccessIdentityAlreadyExists
-- error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig (Text)
cfoaicCallerReference f x =
    f (_cfoaicCallerReference x)
        <&> \y -> x { _cfoaicCallerReference = y }
{-# INLINE cfoaicCallerReference #-}

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig (Text)
cfoaicComment f x =
    f (_cfoaicComment x)
        <&> \y -> x { _cfoaicComment = y }
{-# INLINE cfoaicComment #-}

instance FromXML CloudFrontOriginAccessIdentityConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentityConfig"

instance ToXML CloudFrontOriginAccessIdentityConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CloudFrontOriginAccessIdentityConfig"

-- | The CloudFrontOriginAccessIdentityList type.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList
    { _cfoailMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _cfoailNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your origin access identities where they left off.
    , _cfoailMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _cfoailIsTruncated :: Bool
      -- ^ A flag that indicates whether more origin access identities
      -- remain to be listed. If your results were truncated, you can make
      -- a follow-up pagination request using the Marker request parameter
      -- to retrieve more items in the list.
    , _cfoailQuantity :: Integer
      -- ^ The number of CloudFront origin access identities that were
      -- created by the current AWS account.
    , _cfoailItems :: [CloudFrontOriginAccessIdentitySummary]
      -- ^ A complex type that contains one
      -- CloudFrontOriginAccessIdentitySummary element for each origin
      -- access identity that was created by the current AWS account.
    } deriving (Show, Generic)

-- | The value you provided for the Marker request parameter.
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList (Text)
cfoailMarker f x =
    f (_cfoailMarker x)
        <&> \y -> x { _cfoailMarker = y }
{-# INLINE cfoailMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your origin
-- access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker f x =
    f (_cfoailNextMarker x)
        <&> \y -> x { _cfoailNextMarker = y }
{-# INLINE cfoailNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList (Integer)
cfoailMaxItems f x =
    f (_cfoailMaxItems x)
        <&> \y -> x { _cfoailMaxItems = y }
{-# INLINE cfoailMaxItems #-}

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more items in the
-- list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList (Bool)
cfoailIsTruncated f x =
    f (_cfoailIsTruncated x)
        <&> \y -> x { _cfoailIsTruncated = y }
{-# INLINE cfoailIsTruncated #-}

-- | The number of CloudFront origin access identities that were created by the
-- current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList (Integer)
cfoailQuantity f x =
    f (_cfoailQuantity x)
        <&> \y -> x { _cfoailQuantity = y }
{-# INLINE cfoailQuantity #-}

-- | A complex type that contains one CloudFrontOriginAccessIdentitySummary
-- element for each origin access identity that was created by the current AWS
-- account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList ([CloudFrontOriginAccessIdentitySummary])
cfoailItems f x =
    f (_cfoailItems x)
        <&> \y -> x { _cfoailItems = y }
{-# INLINE cfoailItems #-}

instance FromXML CloudFrontOriginAccessIdentityList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentityList"

-- | Summary of the information about a CloudFront origin access identity.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary
    { _cfoaisId :: Text
      -- ^ The ID for the origin access identity. For example:
      -- E74FTE3AJFJ256A.
    , _cfoaisS3CanonicalUserId :: Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity,
      -- which you use when giving the origin access identity read
      -- permission to an object in Amazon S3.
    , _cfoaisComment :: Text
      -- ^ The comment for this origin access identity, as originally
      -- specified when created.
    } deriving (Show, Generic)

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary (Text)
cfoaisId f x =
    f (_cfoaisId x)
        <&> \y -> x { _cfoaisId = y }
{-# INLINE cfoaisId #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you
-- use when giving the origin access identity read permission to an object in
-- Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary (Text)
cfoaisS3CanonicalUserId f x =
    f (_cfoaisS3CanonicalUserId x)
        <&> \y -> x { _cfoaisS3CanonicalUserId = y }
{-# INLINE cfoaisS3CanonicalUserId #-}

-- | The comment for this origin access identity, as originally specified when
-- created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary (Text)
cfoaisComment f x =
    f (_cfoaisComment x)
        <&> \y -> x { _cfoaisComment = y }
{-# INLINE cfoaisComment #-}

instance FromXML CloudFrontOriginAccessIdentitySummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentitySummary"

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
data CookieNames = CookieNames
    { _cnQuantity :: Integer
      -- ^ The number of whitelisted cookies for this cache behavior.
    , _cnItems :: [Text]
      -- ^ Optional: A complex type that contains whitelisted cookies for
      -- this cache behavior. If Quantity is 0, you can omit Items.
    } deriving (Show, Generic)

-- | The number of whitelisted cookies for this cache behavior.
cnQuantity :: Lens' CookieNames (Integer)
cnQuantity f x =
    f (_cnQuantity x)
        <&> \y -> x { _cnQuantity = y }
{-# INLINE cnQuantity #-}

-- | Optional: A complex type that contains whitelisted cookies for this cache
-- behavior. If Quantity is 0, you can omit Items.
cnItems :: Lens' CookieNames ([Text])
cnItems f x =
    f (_cnItems x)
        <&> \y -> x { _cnItems = y }
{-# INLINE cnItems #-}

instance FromXML CookieNames where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CookieNames"

instance ToXML CookieNames where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CookieNames"

-- | A complex type that specifies how CloudFront handles cookies.
data CookiePreference = CookiePreference
    { _cpForward :: ItemSelection
      -- ^ Use this element to specify whether you want CloudFront to
      -- forward cookies to the origin that is associated with this cache
      -- behavior. You can specify all, none or whitelist. If you choose
      -- All, CloudFront forwards all cookies regardless of how many your
      -- application uses.
    , _cpWhitelistedNames :: Maybe CookieNames
      -- ^ A complex type that specifies the whitelisted cookies, if any,
      -- that you want CloudFront to forward to your origin that is
      -- associated with this cache behavior.
    } deriving (Show, Generic)

-- | Use this element to specify whether you want CloudFront to forward cookies
-- to the origin that is associated with this cache behavior. You can specify
-- all, none or whitelist. If you choose All, CloudFront forwards all cookies
-- regardless of how many your application uses.
cpForward :: Lens' CookiePreference (ItemSelection)
cpForward f x =
    f (_cpForward x)
        <&> \y -> x { _cpForward = y }
{-# INLINE cpForward #-}

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames f x =
    f (_cpWhitelistedNames x)
        <&> \y -> x { _cpWhitelistedNames = y }
{-# INLINE cpWhitelistedNames #-}

instance FromXML CookiePreference where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CookiePreference"

instance ToXML CookiePreference where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CookiePreference"

-- | A complex type that describes how you'd prefer CloudFront to respond to
-- requests that result in either a 4xx or 5xx response. You can control
-- whether a custom error page should be displayed, what the desired response
-- code should be for this error page and how long should the error response
-- be cached by CloudFront. If you don't want to specify any custom error
-- responses, include only an empty CustomErrorResponses element. To delete
-- all custom error responses in an existing distribution, update the
-- distribution configuration and include only an empty CustomErrorResponses
-- element. To add, change, or remove one or more custom error responses,
-- update the distribution configuration and specify all of the custom error
-- responses that you want to include in the updated distribution.
data CustomErrorResponse = CustomErrorResponse
    { _cesErrorCode :: Integer
      -- ^ The 4xx or 5xx HTTP status code that you want to customize. For a
      -- list of HTTP status codes that you can customize, see CloudFront
      -- documentation.
    , _cesResponsePagePath :: Maybe Text
      -- ^ The path of the custom error page (for example,
      -- /custom_404.html). The path is relative to the distribution and
      -- must begin with a slash (/). If the path includes any non-ASCII
      -- characters or unsafe characters as defined in RFC 1783
      -- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
      -- characters. Do not URL encode any other characters in the path,
      -- or CloudFront will not return the custom error page to the
      -- viewer.
    , _cesResponseCode :: Maybe Text
      -- ^ The HTTP status code that you want CloudFront to return with the
      -- custom error page to the viewer. For a list of HTTP status codes
      -- that you can replace, see CloudFront Documentation.
    , _cesErrorCachingMinTTL :: Maybe Integer
      -- ^ The minimum amount of time you want HTTP error codes to stay in
      -- CloudFront caches before CloudFront queries your origin to see
      -- whether the object has been updated. You can specify a value from
      -- 0 to 31,536,000.
    } deriving (Show, Generic)

-- | The 4xx or 5xx HTTP status code that you want to customize. For a list of
-- HTTP status codes that you can customize, see CloudFront documentation.
cesErrorCode :: Lens' CustomErrorResponse (Integer)
cesErrorCode f x =
    f (_cesErrorCode x)
        <&> \y -> x { _cesErrorCode = y }
{-# INLINE cesErrorCode #-}

-- | The path of the custom error page (for example, /custom_404.html). The path
-- is relative to the distribution and must begin with a slash (/). If the
-- path includes any non-ASCII characters or unsafe characters as defined in
-- RFC 1783 (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
-- characters. Do not URL encode any other characters in the path, or
-- CloudFront will not return the custom error page to the viewer.
cesResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
cesResponsePagePath f x =
    f (_cesResponsePagePath x)
        <&> \y -> x { _cesResponsePagePath = y }
{-# INLINE cesResponsePagePath #-}

-- | The HTTP status code that you want CloudFront to return with the custom
-- error page to the viewer. For a list of HTTP status codes that you can
-- replace, see CloudFront Documentation.
cesResponseCode :: Lens' CustomErrorResponse (Maybe Text)
cesResponseCode f x =
    f (_cesResponseCode x)
        <&> \y -> x { _cesResponseCode = y }
{-# INLINE cesResponseCode #-}

-- | The minimum amount of time you want HTTP error codes to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated. You can specify a value from 0 to 31,536,000.
cesErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
cesErrorCachingMinTTL f x =
    f (_cesErrorCachingMinTTL x)
        <&> \y -> x { _cesErrorCachingMinTTL = y }
{-# INLINE cesErrorCachingMinTTL #-}

instance FromXML CustomErrorResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CustomErrorResponse"

instance ToXML CustomErrorResponse where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CustomErrorResponse"

-- | A complex type that contains zero or more CustomErrorResponse elements.
data CustomErrorResponses = CustomErrorResponses
    { _cerQuantity :: Integer
      -- ^ The number of custom error responses for this distribution.
    , _cerItems :: [CustomErrorResponse]
      -- ^ Optional: A complex type that contains custom error responses for
      -- this distribution. If Quantity is 0, you can omit Items.
    } deriving (Show, Generic)

-- | The number of custom error responses for this distribution.
cerQuantity :: Lens' CustomErrorResponses (Integer)
cerQuantity f x =
    f (_cerQuantity x)
        <&> \y -> x { _cerQuantity = y }
{-# INLINE cerQuantity #-}

-- | Optional: A complex type that contains custom error responses for this
-- distribution. If Quantity is 0, you can omit Items.
cerItems :: Lens' CustomErrorResponses ([CustomErrorResponse])
cerItems f x =
    f (_cerItems x)
        <&> \y -> x { _cerItems = y }
{-# INLINE cerItems #-}

instance FromXML CustomErrorResponses where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CustomErrorResponses"

instance ToXML CustomErrorResponses where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CustomErrorResponses"

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
data CustomOriginConfig = CustomOriginConfig
    { _cocHTTPPort :: Integer
      -- ^ The HTTP port the custom origin listens on.
    , _cocHTTPSPort :: Integer
      -- ^ The HTTPS port the custom origin listens on.
    , _cocOriginProtocolPolicy :: OriginProtocolPolicy
      -- ^ The origin protocol policy to apply to your origin.
    } deriving (Show, Generic)

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig (Integer)
cocHTTPPort f x =
    f (_cocHTTPPort x)
        <&> \y -> x { _cocHTTPPort = y }
{-# INLINE cocHTTPPort #-}

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig (Integer)
cocHTTPSPort f x =
    f (_cocHTTPSPort x)
        <&> \y -> x { _cocHTTPSPort = y }
{-# INLINE cocHTTPSPort #-}

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig (OriginProtocolPolicy)
cocOriginProtocolPolicy f x =
    f (_cocOriginProtocolPolicy x)
        <&> \y -> x { _cocOriginProtocolPolicy = y }
{-# INLINE cocOriginProtocolPolicy #-}

instance FromXML CustomOriginConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CustomOriginConfig"

instance ToXML CustomOriginConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CustomOriginConfig"

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
data DefaultCacheBehavior = DefaultCacheBehavior
    { _dcbTargetOriginId :: Text
      -- ^ The value of ID for the origin that you want CloudFront to route
      -- requests to when a request matches the path pattern either for a
      -- cache behavior or for the default cache behavior.
    , _dcbForwardedValues :: ForwardedValues
      -- ^ A complex type that specifies how CloudFront handles query
      -- strings, cookies and headers.
    , _dcbTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you
      -- want to allow to create signed URLs for private content. If you
      -- want to require signed URLs in requests for objects in the target
      -- origin that match the PathPattern for this cache behavior,
      -- specify true for Enabled, and specify the applicable values for
      -- Quantity and Items. For more information, go to Using a Signed
      -- URL to Serve Private Content in the Amazon CloudFront Developer
      -- Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0
      -- for Quantity. Omit Items. To add, change, or remove one or more
      -- trusted signers, change Enabled to true (if it's currently
      -- false), change Quantity as applicable, and specify all of the
      -- trusted signers that you want to include in the updated
      -- distribution.
    , _dcbViewerProtocolPolicy :: ViewerProtocolPolicy
      -- ^ Use this element to specify the protocol that users can use to
      -- access the files in the origin specified by TargetOriginId when a
      -- request matches the path pattern in PathPattern. If you want
      -- CloudFront to allow end users to use any available protocol,
      -- specify allow-all. If you want CloudFront to require HTTPS,
      -- specify https. If you want CloudFront to respond to an HTTP
      -- request with an HTTP status code of 301 (Moved Permanently) and
      -- the HTTPS URL, specify redirect-to-https. The viewer then
      -- resubmits the request using the HTTPS URL.
    , _dcbMinTTL :: Integer
      -- ^ The minimum amount of time that you want objects to stay in
      -- CloudFront caches before CloudFront queries your origin to see
      -- whether the object has been updated.You can specify a value from
      -- 0 to 3,153,600,000 seconds (100 years).
    , _dcbAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront
      -- processes and forwards to your Amazon S3 bucket or your custom
      -- origin. There are two options: - CloudFront forwards only GET and
      -- HEAD requests. - CloudFront forwards DELETE, GET, HEAD, OPTIONS,
      -- PATCH, POST, and PUT requests. If you choose the second option,
      -- you may need to restrict access to your Amazon S3 bucket or to
      -- your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have
      -- permission to delete objects from your origin.
    , _dcbSmoothStreaming :: Maybe Bool
      -- ^ Indicates whether you want to distribute media files in Microsoft
      -- Smooth Streaming format using the origin that is associated with
      -- this cache behavior. If so, specify true; if not, specify false.
    } deriving (Show, Generic)

-- | The value of ID for the origin that you want CloudFront to route requests
-- to when a request matches the path pattern either for a cache behavior or
-- for the default cache behavior.
dcbTargetOriginId :: Lens' DefaultCacheBehavior (Text)
dcbTargetOriginId f x =
    f (_dcbTargetOriginId x)
        <&> \y -> x { _dcbTargetOriginId = y }
{-# INLINE dcbTargetOriginId #-}

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior (ForwardedValues)
dcbForwardedValues f x =
    f (_dcbForwardedValues x)
        <&> \y -> x { _dcbForwardedValues = y }
{-# INLINE dcbForwardedValues #-}

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior (TrustedSigners)
dcbTrustedSigners f x =
    f (_dcbTrustedSigners x)
        <&> \y -> x { _dcbTrustedSigners = y }
{-# INLINE dcbTrustedSigners #-}

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to
-- use any available protocol, specify allow-all. If you want CloudFront to
-- require HTTPS, specify https. If you want CloudFront to respond to an HTTP
-- request with an HTTP status code of 301 (Moved Permanently) and the HTTPS
-- URL, specify redirect-to-https. The viewer then resubmits the request using
-- the HTTPS URL.
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior (ViewerProtocolPolicy)
dcbViewerProtocolPolicy f x =
    f (_dcbViewerProtocolPolicy x)
        <&> \y -> x { _dcbViewerProtocolPolicy = y }
{-# INLINE dcbViewerProtocolPolicy #-}

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
-- years).
dcbMinTTL :: Lens' DefaultCacheBehavior (Integer)
dcbMinTTL f x =
    f (_dcbMinTTL x)
        <&> \y -> x { _dcbMinTTL = y }
{-# INLINE dcbMinTTL #-}

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are two
-- options: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests. If you
-- choose the second option, you may need to restrict access to your Amazon S3
-- bucket or to your custom origin so users can't perform operations that you
-- don't want them to. For example, you may not want users to have permission
-- to delete objects from your origin.
dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods f x =
    f (_dcbAllowedMethods x)
        <&> \y -> x { _dcbAllowedMethods = y }
{-# INLINE dcbAllowedMethods #-}

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming f x =
    f (_dcbSmoothStreaming x)
        <&> \y -> x { _dcbSmoothStreaming = y }
{-# INLINE dcbSmoothStreaming #-}

instance FromXML DefaultCacheBehavior where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DefaultCacheBehavior"

instance ToXML DefaultCacheBehavior where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DefaultCacheBehavior"

-- | The distribution's information.
data Distribution = Distribution
    { _dnId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
    , _dnStatus :: Text
      -- ^ This response element indicates the current status of the
      -- distribution. When the status is Deployed, the distribution's
      -- information is fully propagated throughout the Amazon CloudFront
      -- system.
    , _dnLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _dnInProgressInvalidationBatches :: Integer
      -- ^ The number of invalidation batches currently in progress.
    , _dnDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , _dnActiveTrustedSigners :: ActiveTrustedSigners
      -- ^ CloudFront automatically adds this element to the response only
      -- if you've set up the distribution to serve private content with
      -- signed URLs. The element lists the key pair IDs that CloudFront
      -- is aware of for each trusted signer. The Signer child element
      -- lists the AWS account number of the trusted signer (or an empty
      -- Self element if the signer is you). The Signer element also
      -- includes the IDs of any active key pairs associated with the
      -- trusted signer's AWS account. If no KeyPairId element appears for
      -- a Signer, that signer can't create working signed URLs.
    , _dnDistributionConfig :: DistributionConfig
      -- ^ The current configuration information for the distribution.
    } deriving (Show, Generic)

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dnId :: Lens' Distribution (Text)
dnId f x =
    f (_dnId x)
        <&> \y -> x { _dnId = y }
{-# INLINE dnId #-}

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution's information is fully
-- propagated throughout the Amazon CloudFront system.
dnStatus :: Lens' Distribution (Text)
dnStatus f x =
    f (_dnStatus x)
        <&> \y -> x { _dnStatus = y }
{-# INLINE dnStatus #-}

-- | The date and time the distribution was last modified.
dnLastModifiedTime :: Lens' Distribution (ISO8601)
dnLastModifiedTime f x =
    f (_dnLastModifiedTime x)
        <&> \y -> x { _dnLastModifiedTime = y }
{-# INLINE dnLastModifiedTime #-}

-- | The number of invalidation batches currently in progress.
dnInProgressInvalidationBatches :: Lens' Distribution (Integer)
dnInProgressInvalidationBatches f x =
    f (_dnInProgressInvalidationBatches x)
        <&> \y -> x { _dnInProgressInvalidationBatches = y }
{-# INLINE dnInProgressInvalidationBatches #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dnDomainName :: Lens' Distribution (Text)
dnDomainName f x =
    f (_dnDomainName x)
        <&> \y -> x { _dnDomainName = y }
{-# INLINE dnDomainName #-}

-- | CloudFront automatically adds this element to the response only if you've
-- set up the distribution to serve private content with signed URLs. The
-- element lists the key pair IDs that CloudFront is aware of for each trusted
-- signer. The Signer child element lists the AWS account number of the
-- trusted signer (or an empty Self element if the signer is you). The Signer
-- element also includes the IDs of any active key pairs associated with the
-- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
-- that signer can't create working signed URLs.
dnActiveTrustedSigners :: Lens' Distribution (ActiveTrustedSigners)
dnActiveTrustedSigners f x =
    f (_dnActiveTrustedSigners x)
        <&> \y -> x { _dnActiveTrustedSigners = y }
{-# INLINE dnActiveTrustedSigners #-}

-- | The current configuration information for the distribution.
dnDistributionConfig :: Lens' Distribution (DistributionConfig)
dnDistributionConfig f x =
    f (_dnDistributionConfig x)
        <&> \y -> x { _dnDistributionConfig = y }
{-# INLINE dnDistributionConfig #-}

instance FromXML Distribution where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Distribution"

-- | The distribution's configuration information.
data DistributionConfig = DistributionConfig
    { _dcCallerReference :: Text
      -- ^ A unique number that ensures the request can't be replayed. If
      -- the CallerReference is new (no matter the content of the
      -- DistributionConfig object), a new distribution is created. If the
      -- CallerReference is a value you already sent in a previous request
      -- to create a distribution, and the content of the
      -- DistributionConfig is identical to the original request (ignoring
      -- white space), the response includes the same information returned
      -- to the original request. If the CallerReference is a value you
      -- already sent in a previous request to create a distribution but
      -- the content of the DistributionConfig is different from the
      -- original request, CloudFront returns a DistributionAlreadyExists
      -- error.
    , _dcAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this distribution.
    , _dcDefaultRootObject :: Text
      -- ^ The object that you want CloudFront to return (for example,
      -- index.html) when an end user requests the root URL for your
      -- distribution (http://www.example.com) instead of an object in
      -- your distribution (http://www.example.com/index.html). Specifying
      -- a default root object avoids exposing the contents of your
      -- distribution. If you don't want to specify a default root object
      -- when you create a distribution, include an empty
      -- DefaultRootObject element. To delete the default root object from
      -- an existing distribution, update the distribution configuration
      -- and include an empty DefaultRootObject element. To replace the
      -- default root object, update the distribution configuration and
      -- specify the new object.
    , _dcOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , _dcDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you
      -- do not specify a CacheBehavior element or if files don't match
      -- any of the values of PathPattern in CacheBehavior elements.You
      -- must create exactly one default cache behavior.
    , _dcCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , _dcCustomErrorResponses :: Maybe CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponse
      -- elements.
    , _dcComment :: Text
      -- ^ Any comments you want to include about the distribution.
    , _dcLogging :: LoggingConfig
      -- ^ A complex type that controls whether access logs are written for
      -- the distribution.
    , _dcPriceClass :: PriceClass
      -- ^ A complex type that contains information about price class for
      -- this distribution.
    , _dcEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
    , _dcViewerCertificate :: Maybe ViewerCertificate
      -- ^ A complex type that contains information about viewer
      -- certificates for this distribution.
    , _dcRestrictions :: Maybe Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    } deriving (Show, Generic)

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the DistributionConfig
-- object), a new distribution is created. If the CallerReference is a value
-- you already sent in a previous request to create a distribution, and the
-- content of the DistributionConfig is identical to the original request
-- (ignoring white space), the response includes the same information returned
-- to the original request. If the CallerReference is a value you already sent
-- in a previous request to create a distribution but the content of the
-- DistributionConfig is different from the original request, CloudFront
-- returns a DistributionAlreadyExists error.
dcCallerReference :: Lens' DistributionConfig (Text)
dcCallerReference f x =
    f (_dcCallerReference x)
        <&> \y -> x { _dcCallerReference = y }
{-# INLINE dcCallerReference #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Aliases)
dcAliases f x =
    f (_dcAliases x)
        <&> \y -> x { _dcAliases = y }
{-# INLINE dcAliases #-}

-- | The object that you want CloudFront to return (for example, index.html)
-- when an end user requests the root URL for your distribution
-- (http://www.example.com) instead of an object in your distribution
-- (http://www.example.com/index.html). Specifying a default root object
-- avoids exposing the contents of your distribution. If you don't want to
-- specify a default root object when you create a distribution, include an
-- empty DefaultRootObject element. To delete the default root object from an
-- existing distribution, update the distribution configuration and include an
-- empty DefaultRootObject element. To replace the default root object, update
-- the distribution configuration and specify the new object.
dcDefaultRootObject :: Lens' DistributionConfig (Text)
dcDefaultRootObject f x =
    f (_dcDefaultRootObject x)
        <&> \y -> x { _dcDefaultRootObject = y }
{-# INLINE dcDefaultRootObject #-}

-- | A complex type that contains information about origins for this
-- distribution.
dcOrigins :: Lens' DistributionConfig (Origins)
dcOrigins f x =
    f (_dcOrigins x)
        <&> \y -> x { _dcOrigins = y }
{-# INLINE dcOrigins #-}

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig (DefaultCacheBehavior)
dcDefaultCacheBehavior f x =
    f (_dcDefaultCacheBehavior x)
        <&> \y -> x { _dcDefaultCacheBehavior = y }
{-# INLINE dcDefaultCacheBehavior #-}

-- | A complex type that contains zero or more CacheBehavior elements.
dcCacheBehaviors :: Lens' DistributionConfig (CacheBehaviors)
dcCacheBehaviors f x =
    f (_dcCacheBehaviors x)
        <&> \y -> x { _dcCacheBehaviors = y }
{-# INLINE dcCacheBehaviors #-}

-- | A complex type that contains zero or more CustomErrorResponse elements.
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses f x =
    f (_dcCustomErrorResponses x)
        <&> \y -> x { _dcCustomErrorResponses = y }
{-# INLINE dcCustomErrorResponses #-}

-- | Any comments you want to include about the distribution.
dcComment :: Lens' DistributionConfig (Text)
dcComment f x =
    f (_dcComment x)
        <&> \y -> x { _dcComment = y }
{-# INLINE dcComment #-}

-- | A complex type that controls whether access logs are written for the
-- distribution.
dcLogging :: Lens' DistributionConfig (LoggingConfig)
dcLogging f x =
    f (_dcLogging x)
        <&> \y -> x { _dcLogging = y }
{-# INLINE dcLogging #-}

-- | A complex type that contains information about price class for this
-- distribution.
dcPriceClass :: Lens' DistributionConfig (PriceClass)
dcPriceClass f x =
    f (_dcPriceClass x)
        <&> \y -> x { _dcPriceClass = y }
{-# INLINE dcPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dcEnabled :: Lens' DistributionConfig (Bool)
dcEnabled f x =
    f (_dcEnabled x)
        <&> \y -> x { _dcEnabled = y }
{-# INLINE dcEnabled #-}

-- | A complex type that contains information about viewer certificates for this
-- distribution.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate f x =
    f (_dcViewerCertificate x)
        <&> \y -> x { _dcViewerCertificate = y }
{-# INLINE dcViewerCertificate #-}

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions f x =
    f (_dcRestrictions x)
        <&> \y -> x { _dcRestrictions = y }
{-# INLINE dcRestrictions #-}

instance FromXML DistributionConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionConfig"

instance ToXML DistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DistributionConfig"

-- | The DistributionList type.
data DistributionList = DistributionList
    { _dlMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _dlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your distributions where they left off.
    , _dlMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _dlIsTruncated :: Bool
      -- ^ A flag that indicates whether more distributions remain to be
      -- listed. If your results were truncated, you can make a follow-up
      -- pagination request using the Marker request parameter to retrieve
      -- more distributions in the list.
    , _dlQuantity :: Integer
      -- ^ The number of distributions that were created by the current AWS
      -- account.
    , _dlItems :: [DistributionSummary]
      -- ^ A complex type that contains one DistributionSummary element for
      -- each distribution that was created by the current AWS account.
    } deriving (Show, Generic)

-- | The value you provided for the Marker request parameter.
dlMarker :: Lens' DistributionList (Text)
dlMarker f x =
    f (_dlMarker x)
        <&> \y -> x { _dlMarker = y }
{-# INLINE dlMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker f x =
    f (_dlNextMarker x)
        <&> \y -> x { _dlNextMarker = y }
{-# INLINE dlNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
dlMaxItems :: Lens' DistributionList (Integer)
dlMaxItems f x =
    f (_dlMaxItems x)
        <&> \y -> x { _dlMaxItems = y }
{-# INLINE dlMaxItems #-}

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the Marker request parameter to retrieve more distributions in the
-- list.
dlIsTruncated :: Lens' DistributionList (Bool)
dlIsTruncated f x =
    f (_dlIsTruncated x)
        <&> \y -> x { _dlIsTruncated = y }
{-# INLINE dlIsTruncated #-}

-- | The number of distributions that were created by the current AWS account.
dlQuantity :: Lens' DistributionList (Integer)
dlQuantity f x =
    f (_dlQuantity x)
        <&> \y -> x { _dlQuantity = y }
{-# INLINE dlQuantity #-}

-- | A complex type that contains one DistributionSummary element for each
-- distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList ([DistributionSummary])
dlItems f x =
    f (_dlItems x)
        <&> \y -> x { _dlItems = y }
{-# INLINE dlItems #-}

instance FromXML DistributionList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionList"

-- | A summary of the information for an Amazon CloudFront distribution.
data DistributionSummary = DistributionSummary
    { _dsId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
    , _dsStatus :: Text
      -- ^ This response element indicates the current status of the
      -- distribution. When the status is Deployed, the distribution's
      -- information is fully propagated throughout the Amazon CloudFront
      -- system.
    , _dsLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _dsDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , _dsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this distribution.
    , _dsOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , _dsDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you
      -- do not specify a CacheBehavior element or if files don't match
      -- any of the values of PathPattern in CacheBehavior elements.You
      -- must create exactly one default cache behavior.
    , _dsCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , _dsCustomErrorResponses :: CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponses
      -- elements.
    , _dsComment :: Text
      -- ^ The comment originally specified when this distribution was
      -- created.
    , _dsPriceClass :: PriceClass
    , _dsEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
    , _dsViewerCertificate :: ViewerCertificate
      -- ^ A complex type that contains information about viewer
      -- certificates for this distribution.
    , _dsRestrictions :: Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    } deriving (Show, Generic)

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dsId :: Lens' DistributionSummary (Text)
dsId f x =
    f (_dsId x)
        <&> \y -> x { _dsId = y }
{-# INLINE dsId #-}

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution's information is fully
-- propagated throughout the Amazon CloudFront system.
dsStatus :: Lens' DistributionSummary (Text)
dsStatus f x =
    f (_dsStatus x)
        <&> \y -> x { _dsStatus = y }
{-# INLINE dsStatus #-}

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary (ISO8601)
dsLastModifiedTime f x =
    f (_dsLastModifiedTime x)
        <&> \y -> x { _dsLastModifiedTime = y }
{-# INLINE dsLastModifiedTime #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dsDomainName :: Lens' DistributionSummary (Text)
dsDomainName f x =
    f (_dsDomainName x)
        <&> \y -> x { _dsDomainName = y }
{-# INLINE dsDomainName #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary (Aliases)
dsAliases f x =
    f (_dsAliases x)
        <&> \y -> x { _dsAliases = y }
{-# INLINE dsAliases #-}

-- | A complex type that contains information about origins for this
-- distribution.
dsOrigins :: Lens' DistributionSummary (Origins)
dsOrigins f x =
    f (_dsOrigins x)
        <&> \y -> x { _dsOrigins = y }
{-# INLINE dsOrigins #-}

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary (DefaultCacheBehavior)
dsDefaultCacheBehavior f x =
    f (_dsDefaultCacheBehavior x)
        <&> \y -> x { _dsDefaultCacheBehavior = y }
{-# INLINE dsDefaultCacheBehavior #-}

-- | A complex type that contains zero or more CacheBehavior elements.
dsCacheBehaviors :: Lens' DistributionSummary (CacheBehaviors)
dsCacheBehaviors f x =
    f (_dsCacheBehaviors x)
        <&> \y -> x { _dsCacheBehaviors = y }
{-# INLINE dsCacheBehaviors #-}

-- | A complex type that contains zero or more CustomErrorResponses elements.
dsCustomErrorResponses :: Lens' DistributionSummary (CustomErrorResponses)
dsCustomErrorResponses f x =
    f (_dsCustomErrorResponses x)
        <&> \y -> x { _dsCustomErrorResponses = y }
{-# INLINE dsCustomErrorResponses #-}

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary (Text)
dsComment f x =
    f (_dsComment x)
        <&> \y -> x { _dsComment = y }
{-# INLINE dsComment #-}

dsPriceClass :: Lens' DistributionSummary (PriceClass)
dsPriceClass f x =
    f (_dsPriceClass x)
        <&> \y -> x { _dsPriceClass = y }
{-# INLINE dsPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dsEnabled :: Lens' DistributionSummary (Bool)
dsEnabled f x =
    f (_dsEnabled x)
        <&> \y -> x { _dsEnabled = y }
{-# INLINE dsEnabled #-}

-- | A complex type that contains information about viewer certificates for this
-- distribution.
dsViewerCertificate :: Lens' DistributionSummary (ViewerCertificate)
dsViewerCertificate f x =
    f (_dsViewerCertificate x)
        <&> \y -> x { _dsViewerCertificate = y }
{-# INLINE dsViewerCertificate #-}

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
dsRestrictions :: Lens' DistributionSummary (Restrictions)
dsRestrictions f x =
    f (_dsRestrictions x)
        <&> \y -> x { _dsRestrictions = y }
{-# INLINE dsRestrictions #-}

instance FromXML DistributionSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionSummary"

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
data ForwardedValues = ForwardedValues
    { _fvQueryString :: Bool
      -- ^ Indicates whether you want CloudFront to forward query strings to
      -- the origin that is associated with this cache behavior. If so,
      -- specify true; if not, specify false.
    , _fvCookies :: CookiePreference
      -- ^ A complex type that specifies how CloudFront handles cookies.
    , _fvHeaders :: Maybe Headers
      -- ^ A complex type that specifies the Headers, if any, that you want
      -- CloudFront to vary upon for this cache behavior.
    } deriving (Show, Generic)

-- | Indicates whether you want CloudFront to forward query strings to the
-- origin that is associated with this cache behavior. If so, specify true; if
-- not, specify false.
fvQueryString :: Lens' ForwardedValues (Bool)
fvQueryString f x =
    f (_fvQueryString x)
        <&> \y -> x { _fvQueryString = y }
{-# INLINE fvQueryString #-}

-- | A complex type that specifies how CloudFront handles cookies.
fvCookies :: Lens' ForwardedValues (CookiePreference)
fvCookies f x =
    f (_fvCookies x)
        <&> \y -> x { _fvCookies = y }
{-# INLINE fvCookies #-}

-- | A complex type that specifies the Headers, if any, that you want CloudFront
-- to vary upon for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders f x =
    f (_fvHeaders x)
        <&> \y -> x { _fvHeaders = y }
{-# INLINE fvHeaders #-}

instance FromXML ForwardedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ForwardedValues"

instance ToXML ForwardedValues where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ForwardedValues"

-- | A complex type that controls the countries in which your content is
-- distributed. For more information about geo restriction, go to Customizing
-- Error Responses in the Amazon CloudFront Developer Guide. CloudFront
-- determines the location of your users using MaxMind GeoIP databases. For
-- information about the accuracy of these databases, see How accurate are
-- your GeoIP databases? on the MaxMind website.
data GeoRestriction = GeoRestriction
    { _grRestrictionType :: GeoRestrictionType
      -- ^ The method that you want to use to restrict distribution of your
      -- content by country: - none: No geo restriction is enabled,
      -- meaning access to content is not restricted by client geo
      -- location. - blacklist: The Location elements specify the
      -- countries in which you do not want CloudFront to distribute your
      -- content. - whitelist: The Location elements specify the countries
      -- in which you want CloudFront to distribute your content.
    , _grQuantity :: Integer
      -- ^ When geo restriction is enabled, this is the number of countries
      -- in your whitelist or blacklist. Otherwise, when it is not
      -- enabled, Quantity is 0, and you can omit Items.
    , _grItems :: [Text]
      -- ^ A complex type that contains a Location element for each country
      -- in which you want CloudFront either to distribute your content
      -- (whitelist) or not distribute your content (blacklist). The
      -- Location element is a two-letter, uppercase country code for a
      -- country that you want to include in your blacklist or whitelist.
      -- Include one Location element for each country. CloudFront and
      -- MaxMind both use ISO 3166 country codes. For the current list of
      -- countries and the corresponding codes, see ISO 3166-1-alpha-2
      -- code on the International Organization for Standardization
      -- website. You can also refer to the country list in the CloudFront
      -- console, which includes both country names and codes.
    } deriving (Show, Generic)

-- | The method that you want to use to restrict distribution of your content by
-- country: - none: No geo restriction is enabled, meaning access to content
-- is not restricted by client geo location. - blacklist: The Location
-- elements specify the countries in which you do not want CloudFront to
-- distribute your content. - whitelist: The Location elements specify the
-- countries in which you want CloudFront to distribute your content.
grRestrictionType :: Lens' GeoRestriction (GeoRestrictionType)
grRestrictionType f x =
    f (_grRestrictionType x)
        <&> \y -> x { _grRestrictionType = y }
{-# INLINE grRestrictionType #-}

-- | When geo restriction is enabled, this is the number of countries in your
-- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is 0,
-- and you can omit Items.
grQuantity :: Lens' GeoRestriction (Integer)
grQuantity f x =
    f (_grQuantity x)
        <&> \y -> x { _grQuantity = y }
{-# INLINE grQuantity #-}

-- | A complex type that contains a Location element for each country in which
-- you want CloudFront either to distribute your content (whitelist) or not
-- distribute your content (blacklist). The Location element is a two-letter,
-- uppercase country code for a country that you want to include in your
-- blacklist or whitelist. Include one Location element for each country.
-- CloudFront and MaxMind both use ISO 3166 country codes. For the current
-- list of countries and the corresponding codes, see ISO 3166-1-alpha-2 code
-- on the International Organization for Standardization website. You can also
-- refer to the country list in the CloudFront console, which includes both
-- country names and codes.
grItems :: Lens' GeoRestriction ([Text])
grItems f x =
    f (_grItems x)
        <&> \y -> x { _grItems = y }
{-# INLINE grItems #-}

instance FromXML GeoRestriction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoRestriction"

instance ToXML GeoRestriction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GeoRestriction"

-- | A complex type that specifies the Headers, if any, that you want CloudFront
-- to vary upon for this cache behavior.
data Headers = Headers
    { _hQuantity :: Integer
      -- ^ The number of different headers that you want CloudFront to
      -- forward to the origin and to vary on for this cache behavior. The
      -- maximum number of headers that you can specify by name is 10. If
      -- you want CloudFront to forward all headers to the origin and vary
      -- on all of them, specify 1 for Quantity and * for Name. If you
      -- don't want CloudFront to forward any additional headers to the
      -- origin or to vary on any headers, specify 0 for Quantity and omit
      -- Items.
    , _hItems :: [Text]
      -- ^ Optional: A complex type that contains a Name element for each
      -- header that you want CloudFront to forward to the origin and to
      -- vary on for this cache behavior. If Quantity is 0, omit Items.
    } deriving (Show, Generic)

-- | The number of different headers that you want CloudFront to forward to the
-- origin and to vary on for this cache behavior. The maximum number of
-- headers that you can specify by name is 10. If you want CloudFront to
-- forward all headers to the origin and vary on all of them, specify 1 for
-- Quantity and * for Name. If you don't want CloudFront to forward any
-- additional headers to the origin or to vary on any headers, specify 0 for
-- Quantity and omit Items.
hQuantity :: Lens' Headers (Integer)
hQuantity f x =
    f (_hQuantity x)
        <&> \y -> x { _hQuantity = y }
{-# INLINE hQuantity #-}

-- | Optional: A complex type that contains a Name element for each header that
-- you want CloudFront to forward to the origin and to vary on for this cache
-- behavior. If Quantity is 0, omit Items.
hItems :: Lens' Headers ([Text])
hItems f x =
    f (_hItems x)
        <&> \y -> x { _hItems = y }
{-# INLINE hItems #-}

instance FromXML Headers where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Headers"

instance ToXML Headers where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Headers"

-- | The invalidation's information.
data Invalidation = Invalidation
    { _inId :: Text
      -- ^ The identifier for the invalidation request. For example:
      -- IDFDVBD632BHDS5.
    , _inStatus :: Text
      -- ^ The status of the invalidation request. When the invalidation
      -- batch is finished, the status is Completed.
    , _inCreateTime :: ISO8601
      -- ^ The date and time the invalidation request was first made.
    , _inInvalidationBatch :: InvalidationBatch
      -- ^ The current invalidation information for the batch request.
    } deriving (Show, Generic)

-- | The identifier for the invalidation request. For example: IDFDVBD632BHDS5.
inId :: Lens' Invalidation (Text)
inId f x =
    f (_inId x)
        <&> \y -> x { _inId = y }
{-# INLINE inId #-}

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is Completed.
inStatus :: Lens' Invalidation (Text)
inStatus f x =
    f (_inStatus x)
        <&> \y -> x { _inStatus = y }
{-# INLINE inStatus #-}

-- | The date and time the invalidation request was first made.
inCreateTime :: Lens' Invalidation (ISO8601)
inCreateTime f x =
    f (_inCreateTime x)
        <&> \y -> x { _inCreateTime = y }
{-# INLINE inCreateTime #-}

-- | The current invalidation information for the batch request.
inInvalidationBatch :: Lens' Invalidation (InvalidationBatch)
inInvalidationBatch f x =
    f (_inInvalidationBatch x)
        <&> \y -> x { _inInvalidationBatch = y }
{-# INLINE inInvalidationBatch #-}

instance FromXML Invalidation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Invalidation"

-- | The batch information for the invalidation.
data InvalidationBatch = InvalidationBatch
    { _ibPaths :: Paths
      -- ^ The path of the object to invalidate. The path is relative to the
      -- distribution and must begin with a slash (/). You must enclose
      -- each invalidation object with the Path element tags. If the path
      -- includes non-ASCII characters or unsafe characters as defined in
      -- RFC 1783 (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
      -- characters. Do not URL encode any other characters in the path,
      -- or CloudFront will not invalidate the old version of the updated
      -- object.
    , _ibCallerReference :: Text
      -- ^ A unique name that ensures the request can't be replayed. If the
      -- CallerReference is new (no matter the content of the Path
      -- object), a new distribution is created. If the CallerReference is
      -- a value you already sent in a previous request to create an
      -- invalidation batch, and the content of each Path element is
      -- identical to the original request, the response includes the same
      -- information returned to the original request. If the
      -- CallerReference is a value you already sent in a previous request
      -- to create a distribution but the content of any Path is different
      -- from the original request, CloudFront returns an
      -- InvalidationBatchAlreadyExists error.
    } deriving (Show, Generic)

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
-- URL encode any other characters in the path, or CloudFront will not
-- invalidate the old version of the updated object.
ibPaths :: Lens' InvalidationBatch (Paths)
ibPaths f x =
    f (_ibPaths x)
        <&> \y -> x { _ibPaths = y }
{-# INLINE ibPaths #-}

-- | A unique name that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the Path object), a new
-- distribution is created. If the CallerReference is a value you already sent
-- in a previous request to create an invalidation batch, and the content of
-- each Path element is identical to the original request, the response
-- includes the same information returned to the original request. If the
-- CallerReference is a value you already sent in a previous request to create
-- a distribution but the content of any Path is different from the original
-- request, CloudFront returns an InvalidationBatchAlreadyExists error.
ibCallerReference :: Lens' InvalidationBatch (Text)
ibCallerReference f x =
    f (_ibCallerReference x)
        <&> \y -> x { _ibCallerReference = y }
{-# INLINE ibCallerReference #-}

instance FromXML InvalidationBatch where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InvalidationBatch"

instance ToXML InvalidationBatch where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "InvalidationBatch"

-- | Information about invalidation batches.
data InvalidationList = InvalidationList
    { _ilMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _ilNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your invalidation batches where they left off.
    , _ilMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _ilIsTruncated :: Bool
      -- ^ A flag that indicates whether more invalidation batch requests
      -- remain to be listed. If your results were truncated, you can make
      -- a follow-up pagination request using the Marker request parameter
      -- to retrieve more invalidation batches in the list.
    , _ilQuantity :: Integer
      -- ^ The number of invalidation batches that were created by the
      -- current AWS account.
    , _ilItems :: [InvalidationSummary]
      -- ^ A complex type that contains one InvalidationSummary element for
      -- each invalidation batch that was created by the current AWS
      -- account.
    } deriving (Show, Generic)

-- | The value you provided for the Marker request parameter.
ilMarker :: Lens' InvalidationList (Text)
ilMarker f x =
    f (_ilMarker x)
        <&> \y -> x { _ilMarker = y }
{-# INLINE ilMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker f x =
    f (_ilNextMarker x)
        <&> \y -> x { _ilNextMarker = y }
{-# INLINE ilNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
ilMaxItems :: Lens' InvalidationList (Integer)
ilMaxItems f x =
    f (_ilMaxItems x)
        <&> \y -> x { _ilMaxItems = y }
{-# INLINE ilMaxItems #-}

-- | A flag that indicates whether more invalidation batch requests remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more invalidation
-- batches in the list.
ilIsTruncated :: Lens' InvalidationList (Bool)
ilIsTruncated f x =
    f (_ilIsTruncated x)
        <&> \y -> x { _ilIsTruncated = y }
{-# INLINE ilIsTruncated #-}

-- | The number of invalidation batches that were created by the current AWS
-- account.
ilQuantity :: Lens' InvalidationList (Integer)
ilQuantity f x =
    f (_ilQuantity x)
        <&> \y -> x { _ilQuantity = y }
{-# INLINE ilQuantity #-}

-- | A complex type that contains one InvalidationSummary element for each
-- invalidation batch that was created by the current AWS account.
ilItems :: Lens' InvalidationList ([InvalidationSummary])
ilItems f x =
    f (_ilItems x)
        <&> \y -> x { _ilItems = y }
{-# INLINE ilItems #-}

instance FromXML InvalidationList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InvalidationList"

-- | Summary of an invalidation request.
data InvalidationSummary = InvalidationSummary
    { _iiiiiiiiiiiiyId :: Text
      -- ^ The unique ID for an invalidation request.
    , _iiiiiiiiiiiiyCreateTime :: ISO8601
    , _iiiiiiiiiiiiyStatus :: Text
      -- ^ The status of an invalidation request.
    } deriving (Show, Generic)

-- | The unique ID for an invalidation request.
iiiiiiiiiiiiyId :: Lens' InvalidationSummary (Text)
iiiiiiiiiiiiyId f x =
    f (_iiiiiiiiiiiiyId x)
        <&> \y -> x { _iiiiiiiiiiiiyId = y }
{-# INLINE iiiiiiiiiiiiyId #-}

iiiiiiiiiiiiyCreateTime :: Lens' InvalidationSummary (ISO8601)
iiiiiiiiiiiiyCreateTime f x =
    f (_iiiiiiiiiiiiyCreateTime x)
        <&> \y -> x { _iiiiiiiiiiiiyCreateTime = y }
{-# INLINE iiiiiiiiiiiiyCreateTime #-}

-- | The status of an invalidation request.
iiiiiiiiiiiiyStatus :: Lens' InvalidationSummary (Text)
iiiiiiiiiiiiyStatus f x =
    f (_iiiiiiiiiiiiyStatus x)
        <&> \y -> x { _iiiiiiiiiiiiyStatus = y }
{-# INLINE iiiiiiiiiiiiyStatus #-}

instance FromXML InvalidationSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InvalidationSummary"

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
data KeyPairIds = KeyPairIds
    { _kpiQuantity :: Integer
      -- ^ The number of active CloudFront key pairs for AwsAccountNumber.
    , _kpiItems :: [Text]
      -- ^ A complex type that lists the active CloudFront key pairs, if
      -- any, that are associated with AwsAccountNumber.
    } deriving (Show, Generic)

-- | The number of active CloudFront key pairs for AwsAccountNumber.
kpiQuantity :: Lens' KeyPairIds (Integer)
kpiQuantity f x =
    f (_kpiQuantity x)
        <&> \y -> x { _kpiQuantity = y }
{-# INLINE kpiQuantity #-}

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
kpiItems :: Lens' KeyPairIds ([Text])
kpiItems f x =
    f (_kpiItems x)
        <&> \y -> x { _kpiItems = y }
{-# INLINE kpiItems #-}

instance FromXML KeyPairIds where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "KeyPairIds"

instance ToXML KeyPairIds where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "KeyPairIds"

-- | A complex type that controls whether access logs are written for the
-- distribution.
data LoggingConfig = LoggingConfig
    { _lcEnabled :: Bool
      -- ^ Specifies whether you want CloudFront to save access logs to an
      -- Amazon S3 bucket. If you do not want to enable logging when you
      -- create a distribution or if you want to disable logging for an
      -- existing distribution, specify false for Enabled, and specify
      -- empty Bucket and Prefix elements. If you specify false for
      -- Enabled but you specify values for Bucket, prefix and
      -- IncludeCookies, the values are automatically deleted.
    , _lcIncludeCookies :: Bool
      -- ^ Specifies whether you want CloudFront to include cookies in
      -- access logs, specify true for IncludeCookies. If you choose to
      -- include cookies in logs, CloudFront logs all cookies regardless
      -- of how you configure the cache behaviors for this distribution.
      -- If you do not want to include cookies when you create a
      -- distribution or if you want to disable include cookies for an
      -- existing distribution, specify false for IncludeCookies.
    , _lcBucket :: Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    , _lcPrefix :: Text
      -- ^ An optional string that you want CloudFront to prefix to the
      -- access log filenames for this distribution, for example,
      -- myprefix/. If you want to enable logging, but you do not want to
      -- specify a prefix, you still must include an empty Prefix element
      -- in the Logging element.
    } deriving (Show, Generic)

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3
-- bucket. If you do not want to enable logging when you create a distribution
-- or if you want to disable logging for an existing distribution, specify
-- false for Enabled, and specify empty Bucket and Prefix elements. If you
-- specify false for Enabled but you specify values for Bucket, prefix and
-- IncludeCookies, the values are automatically deleted.
lcEnabled :: Lens' LoggingConfig (Bool)
lcEnabled f x =
    f (_lcEnabled x)
        <&> \y -> x { _lcEnabled = y }
{-# INLINE lcEnabled #-}

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify true for IncludeCookies. If you choose to include cookies in logs,
-- CloudFront logs all cookies regardless of how you configure the cache
-- behaviors for this distribution. If you do not want to include cookies when
-- you create a distribution or if you want to disable include cookies for an
-- existing distribution, specify false for IncludeCookies.
lcIncludeCookies :: Lens' LoggingConfig (Bool)
lcIncludeCookies f x =
    f (_lcIncludeCookies x)
        <&> \y -> x { _lcIncludeCookies = y }
{-# INLINE lcIncludeCookies #-}

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
lcBucket :: Lens' LoggingConfig (Text)
lcBucket f x =
    f (_lcBucket x)
        <&> \y -> x { _lcBucket = y }
{-# INLINE lcBucket #-}

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this distribution, for example, myprefix/. If you want to
-- enable logging, but you do not want to specify a prefix, you still must
-- include an empty Prefix element in the Logging element.
lcPrefix :: Lens' LoggingConfig (Text)
lcPrefix f x =
    f (_lcPrefix x)
        <&> \y -> x { _lcPrefix = y }
{-# INLINE lcPrefix #-}

instance FromXML LoggingConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoggingConfig"

instance ToXML LoggingConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "LoggingConfig"

-- | A complex type that describes the Amazon S3 bucket or the HTTP server (for
-- example, a web server) from which CloudFront gets your files.You must
-- create at least one origin.
data Origin = Origin
    { _pId :: Text
      -- ^ A unique identifier for the origin. The value of Id must be
      -- unique within the distribution. You use the value of Id when you
      -- create a cache behavior. The Id identifies the origin that
      -- CloudFront routes a request to when the request matches the path
      -- pattern for that cache behavior.
    , _pDomainName :: Text
      -- ^ Amazon S3 origins: The DNS name of the Amazon S3 bucket from
      -- which you want CloudFront to get objects for this origin, for
      -- example, myawsbucket.s3.amazonaws.com. Custom origins: The DNS
      -- domain name for the HTTP server from which you want CloudFront to
      -- get objects for this origin, for example, www.example.com.
    , _pS3OriginConfig :: Maybe S3OriginConfig
      -- ^ A complex type that contains information about the Amazon S3
      -- origin. If the origin is a custom origin, use the
      -- CustomOriginConfig element instead.
    , _pCustomOriginConfig :: Maybe CustomOriginConfig
      -- ^ A complex type that contains information about a custom origin.
      -- If the origin is an Amazon S3 bucket, use the S3OriginConfig
      -- element instead.
    } deriving (Show, Generic)

-- | A unique identifier for the origin. The value of Id must be unique within
-- the distribution. You use the value of Id when you create a cache behavior.
-- The Id identifies the origin that CloudFront routes a request to when the
-- request matches the path pattern for that cache behavior.
pId :: Lens' Origin (Text)
pId f x =
    f (_pId x)
        <&> \y -> x { _pId = y }
{-# INLINE pId #-}

-- | Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you want
-- CloudFront to get objects for this origin, for example,
-- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for the
-- HTTP server from which you want CloudFront to get objects for this origin,
-- for example, www.example.com.
pDomainName :: Lens' Origin (Text)
pDomainName f x =
    f (_pDomainName x)
        <&> \y -> x { _pDomainName = y }
{-# INLINE pDomainName #-}

-- | A complex type that contains information about the Amazon S3 origin. If the
-- origin is a custom origin, use the CustomOriginConfig element instead.
pS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
pS3OriginConfig f x =
    f (_pS3OriginConfig x)
        <&> \y -> x { _pS3OriginConfig = y }
{-# INLINE pS3OriginConfig #-}

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
pCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
pCustomOriginConfig f x =
    f (_pCustomOriginConfig x)
        <&> \y -> x { _pCustomOriginConfig = y }
{-# INLINE pCustomOriginConfig #-}

instance FromXML Origin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Origin"

instance ToXML Origin where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Origin"

-- | A complex type that contains information about origins for this
-- distribution.
data Origins = Origins
    { _oQuantity :: Integer
      -- ^ The number of origins for this distribution.
    , _oItems :: Maybe [Origin]
      -- ^ A complex type that contains origins for this distribution.
    } deriving (Show, Generic)

-- | The number of origins for this distribution.
oQuantity :: Lens' Origins (Integer)
oQuantity f x =
    f (_oQuantity x)
        <&> \y -> x { _oQuantity = y }
{-# INLINE oQuantity #-}

-- | A complex type that contains origins for this distribution.
oItems :: Lens' Origins (Maybe [Origin])
oItems f x =
    f (_oItems x)
        <&> \y -> x { _oItems = y }
{-# INLINE oItems #-}

instance FromXML Origins where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Origins"

instance ToXML Origins where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Origins"

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
-- URL encode any other characters in the path, or CloudFront will not
-- invalidate the old version of the updated object.
data Paths = Paths
    { _psQuantity :: Integer
      -- ^ The number of objects that you want to invalidate.
    , _psItems :: [Text]
      -- ^ A complex type that contains a list of the objects that you want
      -- to invalidate.
    } deriving (Show, Generic)

-- | The number of objects that you want to invalidate.
psQuantity :: Lens' Paths (Integer)
psQuantity f x =
    f (_psQuantity x)
        <&> \y -> x { _psQuantity = y }
{-# INLINE psQuantity #-}

-- | A complex type that contains a list of the objects that you want to
-- invalidate.
psItems :: Lens' Paths ([Text])
psItems f x =
    f (_psItems x)
        <&> \y -> x { _psItems = y }
{-# INLINE psItems #-}

instance FromXML Paths where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Paths"

instance ToXML Paths where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Paths"

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
data S3Origin = S3Origin
    { _ssssnDomainName :: Text
      -- ^ The DNS name of the S3 origin.
    , _ssssnOriginAccessIdentity :: Text
      -- ^ Your S3 origin's origin access identity.
    } deriving (Show, Generic)

-- | The DNS name of the S3 origin.
ssssnDomainName :: Lens' S3Origin (Text)
ssssnDomainName f x =
    f (_ssssnDomainName x)
        <&> \y -> x { _ssssnDomainName = y }
{-# INLINE ssssnDomainName #-}

-- | Your S3 origin's origin access identity.
ssssnOriginAccessIdentity :: Lens' S3Origin (Text)
ssssnOriginAccessIdentity f x =
    f (_ssssnOriginAccessIdentity x)
        <&> \y -> x { _ssssnOriginAccessIdentity = y }
{-# INLINE ssssnOriginAccessIdentity #-}

instance FromXML S3Origin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Origin"

instance ToXML S3Origin where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "S3Origin"

-- | A complex type that lists the AWS accounts that were included in the
-- TrustedSigners complex type, as well as their active CloudFront key pair
-- IDs, if any.
data Signer = Signer
    { _ssrAwsAccountNumber :: Maybe Text
      -- ^ Specifies an AWS account that can create signed URLs. Values:
      -- self, which indicates that the AWS account that was used to
      -- create the distribution can created signed URLs, or an AWS
      -- account number. Omit the dashes in the account number.
    , _ssrKeyPairIds :: Maybe KeyPairIds
      -- ^ A complex type that lists the active CloudFront key pairs, if
      -- any, that are associated with AwsAccountNumber.
    } deriving (Show, Generic)

-- | Specifies an AWS account that can create signed URLs. Values: self, which
-- indicates that the AWS account that was used to create the distribution can
-- created signed URLs, or an AWS account number. Omit the dashes in the
-- account number.
ssrAwsAccountNumber :: Lens' Signer (Maybe Text)
ssrAwsAccountNumber f x =
    f (_ssrAwsAccountNumber x)
        <&> \y -> x { _ssrAwsAccountNumber = y }
{-# INLINE ssrAwsAccountNumber #-}

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
ssrKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
ssrKeyPairIds f x =
    f (_ssrKeyPairIds x)
        <&> \y -> x { _ssrKeyPairIds = y }
{-# INLINE ssrKeyPairIds #-}

instance FromXML Signer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Signer"

-- | The streaming distribution's information.
data StreamingDistribution = StreamingDistribution
    { _sdId :: Text
      -- ^ The identifier for the streaming distribution. For example:
      -- EGTXBD79H29TRA8.
    , _sdStatus :: Text
      -- ^ The current status of the streaming distribution. When the status
      -- is Deployed, the distribution's information is fully propagated
      -- throughout the Amazon CloudFront system.
    , _sdLastModifiedTime :: Maybe ISO8601
      -- ^ The date and time the distribution was last modified.
    , _sdDomainName :: Text
      -- ^ The domain name corresponding to the streaming distribution. For
      -- example: s5c39gqb8ow64r.cloudfront.net.
    , _sdActiveTrustedSigners :: ActiveTrustedSigners
      -- ^ CloudFront automatically adds this element to the response only
      -- if you've set up the distribution to serve private content with
      -- signed URLs. The element lists the key pair IDs that CloudFront
      -- is aware of for each trusted signer. The Signer child element
      -- lists the AWS account number of the trusted signer (or an empty
      -- Self element if the signer is you). The Signer element also
      -- includes the IDs of any active key pairs associated with the
      -- trusted signer's AWS account. If no KeyPairId element appears for
      -- a Signer, that signer can't create working signed URLs.
    , _sdStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The current configuration information for the streaming
      -- distribution.
    } deriving (Show, Generic)

-- | The identifier for the streaming distribution. For example:
-- EGTXBD79H29TRA8.
sdId :: Lens' StreamingDistribution (Text)
sdId f x =
    f (_sdId x)
        <&> \y -> x { _sdId = y }
{-# INLINE sdId #-}

-- | The current status of the streaming distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdStatus :: Lens' StreamingDistribution (Text)
sdStatus f x =
    f (_sdStatus x)
        <&> \y -> x { _sdStatus = y }
{-# INLINE sdStatus #-}

-- | The date and time the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe ISO8601)
sdLastModifiedTime f x =
    f (_sdLastModifiedTime x)
        <&> \y -> x { _sdLastModifiedTime = y }
{-# INLINE sdLastModifiedTime #-}

-- | The domain name corresponding to the streaming distribution. For example:
-- s5c39gqb8ow64r.cloudfront.net.
sdDomainName :: Lens' StreamingDistribution (Text)
sdDomainName f x =
    f (_sdDomainName x)
        <&> \y -> x { _sdDomainName = y }
{-# INLINE sdDomainName #-}

-- | CloudFront automatically adds this element to the response only if you've
-- set up the distribution to serve private content with signed URLs. The
-- element lists the key pair IDs that CloudFront is aware of for each trusted
-- signer. The Signer child element lists the AWS account number of the
-- trusted signer (or an empty Self element if the signer is you). The Signer
-- element also includes the IDs of any active key pairs associated with the
-- trusted signer's AWS account. If no KeyPairId element appears for a Signer,
-- that signer can't create working signed URLs.
sdActiveTrustedSigners :: Lens' StreamingDistribution (ActiveTrustedSigners)
sdActiveTrustedSigners f x =
    f (_sdActiveTrustedSigners x)
        <&> \y -> x { _sdActiveTrustedSigners = y }
{-# INLINE sdActiveTrustedSigners #-}

-- | The current configuration information for the streaming distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution (StreamingDistributionConfig)
sdStreamingDistributionConfig f x =
    f (_sdStreamingDistributionConfig x)
        <&> \y -> x { _sdStreamingDistributionConfig = y }
{-# INLINE sdStreamingDistributionConfig #-}

instance FromXML StreamingDistribution where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistribution"

-- | The streaming distribution's configuration information.
data StreamingDistributionConfig = StreamingDistributionConfig
    { _sdcCallerReference :: Text
      -- ^ A unique number that ensures the request can't be replayed. If
      -- the CallerReference is new (no matter the content of the
      -- StreamingDistributionConfig object), a new streaming distribution
      -- is created. If the CallerReference is a value you already sent in
      -- a previous request to create a streaming distribution, and the
      -- content of the StreamingDistributionConfig is identical to the
      -- original request (ignoring white space), the response includes
      -- the same information returned to the original request. If the
      -- CallerReference is a value you already sent in a previous request
      -- to create a streaming distribution but the content of the
      -- StreamingDistributionConfig is different from the original
      -- request, CloudFront returns a DistributionAlreadyExists error.
    , _sdcS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3
      -- bucket from which you want CloudFront to get your media files for
      -- distribution.
    , _sdcAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this streaming distribution.
    , _sdcComment :: Text
      -- ^ Any comments you want to include about the streaming
      -- distribution.
    , _sdcLogging :: StreamingLoggingConfig
      -- ^ A complex type that controls whether access logs are written for
      -- the streaming distribution.
    , _sdcTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you
      -- want to allow to create signed URLs for private content. If you
      -- want to require signed URLs in requests for objects in the target
      -- origin that match the PathPattern for this cache behavior,
      -- specify true for Enabled, and specify the applicable values for
      -- Quantity and Items. For more information, go to Using a Signed
      -- URL to Serve Private Content in the Amazon CloudFront Developer
      -- Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0
      -- for Quantity. Omit Items. To add, change, or remove one or more
      -- trusted signers, change Enabled to true (if it's currently
      -- false), change Quantity as applicable, and specify all of the
      -- trusted signers that you want to include in the updated
      -- distribution.
    , _sdcPriceClass :: PriceClass
      -- ^ A complex type that contains information about price class for
      -- this streaming distribution.
    , _sdcEnabled :: Bool
      -- ^ Whether the streaming distribution is enabled to accept end user
      -- requests for content.
    } deriving (Show, Generic)

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the
-- StreamingDistributionConfig object), a new streaming distribution is
-- created. If the CallerReference is a value you already sent in a previous
-- request to create a streaming distribution, and the content of the
-- StreamingDistributionConfig is identical to the original request (ignoring
-- white space), the response includes the same information returned to the
-- original request. If the CallerReference is a value you already sent in a
-- previous request to create a streaming distribution but the content of the
-- StreamingDistributionConfig is different from the original request,
-- CloudFront returns a DistributionAlreadyExists error.
sdcCallerReference :: Lens' StreamingDistributionConfig (Text)
sdcCallerReference f x =
    f (_sdcCallerReference x)
        <&> \y -> x { _sdcCallerReference = y }
{-# INLINE sdcCallerReference #-}

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig (S3Origin)
sdcS3Origin f x =
    f (_sdcS3Origin x)
        <&> \y -> x { _sdcS3Origin = y }
{-# INLINE sdcS3Origin #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Aliases)
sdcAliases f x =
    f (_sdcAliases x)
        <&> \y -> x { _sdcAliases = y }
{-# INLINE sdcAliases #-}

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig (Text)
sdcComment f x =
    f (_sdcComment x)
        <&> \y -> x { _sdcComment = y }
{-# INLINE sdcComment #-}

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (StreamingLoggingConfig)
sdcLogging f x =
    f (_sdcLogging x)
        <&> \y -> x { _sdcLogging = y }
{-# INLINE sdcLogging #-}

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
sdcTrustedSigners :: Lens' StreamingDistributionConfig (TrustedSigners)
sdcTrustedSigners f x =
    f (_sdcTrustedSigners x)
        <&> \y -> x { _sdcTrustedSigners = y }
{-# INLINE sdcTrustedSigners #-}

-- | A complex type that contains information about price class for this
-- streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (PriceClass)
sdcPriceClass f x =
    f (_sdcPriceClass x)
        <&> \y -> x { _sdcPriceClass = y }
{-# INLINE sdcPriceClass #-}

-- | Whether the streaming distribution is enabled to accept end user requests
-- for content.
sdcEnabled :: Lens' StreamingDistributionConfig (Bool)
sdcEnabled f x =
    f (_sdcEnabled x)
        <&> \y -> x { _sdcEnabled = y }
{-# INLINE sdcEnabled #-}

instance FromXML StreamingDistributionConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistributionConfig"

instance ToXML StreamingDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StreamingDistributionConfig"

-- | The StreamingDistributionList type.
data StreamingDistributionList = StreamingDistributionList
    { _sdlMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _sdlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your streaming distributions where they left off.
    , _sdlMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _sdlIsTruncated :: Bool
      -- ^ A flag that indicates whether more streaming distributions remain
      -- to be listed. If your results were truncated, you can make a
      -- follow-up pagination request using the Marker request parameter
      -- to retrieve more distributions in the list.
    , _sdlQuantity :: Integer
      -- ^ The number of streaming distributions that were created by the
      -- current AWS account.
    , _sdlItems :: [StreamingDistributionSummary]
      -- ^ A complex type that contains one StreamingDistributionSummary
      -- element for each distribution that was created by the current AWS
      -- account.
    } deriving (Show, Generic)

-- | The value you provided for the Marker request parameter.
sdlMarker :: Lens' StreamingDistributionList (Text)
sdlMarker f x =
    f (_sdlMarker x)
        <&> \y -> x { _sdlMarker = y }
{-# INLINE sdlMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your streaming
-- distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker f x =
    f (_sdlNextMarker x)
        <&> \y -> x { _sdlNextMarker = y }
{-# INLINE sdlNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
sdlMaxItems :: Lens' StreamingDistributionList (Integer)
sdlMaxItems f x =
    f (_sdlMaxItems x)
        <&> \y -> x { _sdlMaxItems = y }
{-# INLINE sdlMaxItems #-}

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more distributions
-- in the list.
sdlIsTruncated :: Lens' StreamingDistributionList (Bool)
sdlIsTruncated f x =
    f (_sdlIsTruncated x)
        <&> \y -> x { _sdlIsTruncated = y }
{-# INLINE sdlIsTruncated #-}

-- | The number of streaming distributions that were created by the current AWS
-- account.
sdlQuantity :: Lens' StreamingDistributionList (Integer)
sdlQuantity f x =
    f (_sdlQuantity x)
        <&> \y -> x { _sdlQuantity = y }
{-# INLINE sdlQuantity #-}

-- | A complex type that contains one StreamingDistributionSummary element for
-- each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList ([StreamingDistributionSummary])
sdlItems f x =
    f (_sdlItems x)
        <&> \y -> x { _sdlItems = y }
{-# INLINE sdlItems #-}

instance FromXML StreamingDistributionList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistributionList"

-- | A summary of the information for an Amazon CloudFront streaming
-- distribution.
data StreamingDistributionSummary = StreamingDistributionSummary
    { _sdsId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
    , _sdsStatus :: Text
      -- ^ Indicates the current status of the distribution. When the status
      -- is Deployed, the distribution's information is fully propagated
      -- throughout the Amazon CloudFront system.
    , _sdsLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _sdsDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , _sdsS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3
      -- bucket from which you want CloudFront to get your media files for
      -- distribution.
    , _sdsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this streaming distribution.
    , _sdsTrustedSigners :: TrustedSigners
      -- ^ A complex type that specifies the AWS accounts, if any, that you
      -- want to allow to create signed URLs for private content. If you
      -- want to require signed URLs in requests for objects in the target
      -- origin that match the PathPattern for this cache behavior,
      -- specify true for Enabled, and specify the applicable values for
      -- Quantity and Items. For more information, go to Using a Signed
      -- URL to Serve Private Content in the Amazon CloudFront Developer
      -- Guide. If you don't want to require signed URLs in requests for
      -- objects that match PathPattern, specify false for Enabled and 0
      -- for Quantity. Omit Items. To add, change, or remove one or more
      -- trusted signers, change Enabled to true (if it's currently
      -- false), change Quantity as applicable, and specify all of the
      -- trusted signers that you want to include in the updated
      -- distribution.
    , _sdsComment :: Text
      -- ^ The comment originally specified when this distribution was
      -- created.
    , _sdsPriceClass :: PriceClass
    , _sdsEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
    } deriving (Show, Generic)

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
sdsId :: Lens' StreamingDistributionSummary (Text)
sdsId f x =
    f (_sdsId x)
        <&> \y -> x { _sdsId = y }
{-# INLINE sdsId #-}

-- | Indicates the current status of the distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary (Text)
sdsStatus f x =
    f (_sdsStatus x)
        <&> \y -> x { _sdsStatus = y }
{-# INLINE sdsStatus #-}

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary (ISO8601)
sdsLastModifiedTime f x =
    f (_sdsLastModifiedTime x)
        <&> \y -> x { _sdsLastModifiedTime = y }
{-# INLINE sdsLastModifiedTime #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
sdsDomainName :: Lens' StreamingDistributionSummary (Text)
sdsDomainName f x =
    f (_sdsDomainName x)
        <&> \y -> x { _sdsDomainName = y }
{-# INLINE sdsDomainName #-}

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary (S3Origin)
sdsS3Origin f x =
    f (_sdsS3Origin x)
        <&> \y -> x { _sdsS3Origin = y }
{-# INLINE sdsS3Origin #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary (Aliases)
sdsAliases f x =
    f (_sdsAliases x)
        <&> \y -> x { _sdsAliases = y }
{-# INLINE sdsAliases #-}

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
sdsTrustedSigners :: Lens' StreamingDistributionSummary (TrustedSigners)
sdsTrustedSigners f x =
    f (_sdsTrustedSigners x)
        <&> \y -> x { _sdsTrustedSigners = y }
{-# INLINE sdsTrustedSigners #-}

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary (Text)
sdsComment f x =
    f (_sdsComment x)
        <&> \y -> x { _sdsComment = y }
{-# INLINE sdsComment #-}

sdsPriceClass :: Lens' StreamingDistributionSummary (PriceClass)
sdsPriceClass f x =
    f (_sdsPriceClass x)
        <&> \y -> x { _sdsPriceClass = y }
{-# INLINE sdsPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
sdsEnabled :: Lens' StreamingDistributionSummary (Bool)
sdsEnabled f x =
    f (_sdsEnabled x)
        <&> \y -> x { _sdsEnabled = y }
{-# INLINE sdsEnabled #-}

instance FromXML StreamingDistributionSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistributionSummary"

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
data StreamingLoggingConfig = StreamingLoggingConfig
    { _slcEnabled :: Bool
      -- ^ Specifies whether you want CloudFront to save access logs to an
      -- Amazon S3 bucket. If you do not want to enable logging when you
      -- create a streaming distribution or if you want to disable logging
      -- for an existing streaming distribution, specify false for
      -- Enabled, and specify empty Bucket and Prefix elements. If you
      -- specify false for Enabled but you specify values for Bucket and
      -- Prefix, the values are automatically deleted.
    , _slcBucket :: Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    , _slcPrefix :: Text
      -- ^ An optional string that you want CloudFront to prefix to the
      -- access log filenames for this streaming distribution, for
      -- example, myprefix/. If you want to enable logging, but you do not
      -- want to specify a prefix, you still must include an empty Prefix
      -- element in the Logging element.
    } deriving (Show, Generic)

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3
-- bucket. If you do not want to enable logging when you create a streaming
-- distribution or if you want to disable logging for an existing streaming
-- distribution, specify false for Enabled, and specify empty Bucket and
-- Prefix elements. If you specify false for Enabled but you specify values
-- for Bucket and Prefix, the values are automatically deleted.
slcEnabled :: Lens' StreamingLoggingConfig (Bool)
slcEnabled f x =
    f (_slcEnabled x)
        <&> \y -> x { _slcEnabled = y }
{-# INLINE slcEnabled #-}

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
slcBucket :: Lens' StreamingLoggingConfig (Text)
slcBucket f x =
    f (_slcBucket x)
        <&> \y -> x { _slcBucket = y }
{-# INLINE slcBucket #-}

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, myprefix/. If you
-- want to enable logging, but you do not want to specify a prefix, you still
-- must include an empty Prefix element in the Logging element.
slcPrefix :: Lens' StreamingLoggingConfig (Text)
slcPrefix f x =
    f (_slcPrefix x)
        <&> \y -> x { _slcPrefix = y }
{-# INLINE slcPrefix #-}

instance FromXML StreamingLoggingConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingLoggingConfig"

instance ToXML StreamingLoggingConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StreamingLoggingConfig"

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for
-- Quantity. Omit Items. To add, change, or remove one or more trusted
-- signers, change Enabled to true (if it's currently false), change Quantity
-- as applicable, and specify all of the trusted signers that you want to
-- include in the updated distribution.
data TrustedSigners = TrustedSigners
    { _tsEnabled :: Bool
      -- ^ Specifies whether you want to require end users to use signed
      -- URLs to access the files specified by PathPattern and
      -- TargetOriginId.
    , _tsQuantity :: Integer
      -- ^ The number of trusted signers for this cache behavior.
    , _tsItems :: [Text]
      -- ^ Optional: A complex type that contains trusted signers for this
      -- cache behavior. If Quantity is 0, you can omit Items.
    } deriving (Show, Generic)

-- | Specifies whether you want to require end users to use signed URLs to
-- access the files specified by PathPattern and TargetOriginId.
tsEnabled :: Lens' TrustedSigners (Bool)
tsEnabled f x =
    f (_tsEnabled x)
        <&> \y -> x { _tsEnabled = y }
{-# INLINE tsEnabled #-}

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners (Integer)
tsQuantity f x =
    f (_tsQuantity x)
        <&> \y -> x { _tsQuantity = y }
{-# INLINE tsQuantity #-}

-- | Optional: A complex type that contains trusted signers for this cache
-- behavior. If Quantity is 0, you can omit Items.
tsItems :: Lens' TrustedSigners ([Text])
tsItems f x =
    f (_tsItems x)
        <&> \y -> x { _tsItems = y }
{-# INLINE tsItems #-}

instance FromXML TrustedSigners where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TrustedSigners"

instance ToXML TrustedSigners where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TrustedSigners"

-- | A complex type that contains information about viewer certificates for this
-- distribution.
data ViewerCertificate = ViewerCertificate
    { _vcIAMCertificateId :: Maybe Text
      -- ^ If you want viewers to use HTTPS to request your objects and
      -- you're using an alternate domain name in your object URLs (for
      -- example, https://example.com/logo.jpg), specify the IAM
      -- certificate identifier of the custom viewer certificate for this
      -- distribution. Specify either this value or
      -- CloudFrontDefaultCertificate.
    , _vcCloudFrontDefaultCertificate :: Maybe Bool
      -- ^ If you want viewers to use HTTPS to request your objects and
      -- you're using the CloudFront domain name of your distribution in
      -- your object URLs (for example,
      -- https://d111111abcdef8.cloudfront.net/logo.jpg), set to true.
      -- Omit this value if you are setting an IAMCertificateId.
    , _vcSSLSupportMethod :: Maybe SSLSupportMethod
      -- ^ If you specify a value for IAMCertificateId, you must also
      -- specify how you want CloudFront to serve HTTPS requests. Valid
      -- values are vip and sni-only. If you specify vip, CloudFront uses
      -- dedicated IP addresses for your content and can respond to HTTPS
      -- requests from any viewer. However, you must request permission to
      -- use this feature, and you incur additional monthly charges. If
      -- you specify sni-only, CloudFront can only respond to HTTPS
      -- requests from viewers that support Server Name Indication (SNI).
      -- All modern browsers support SNI, but some browsers still in use
      -- don't support SNI. Do not specify a value for SSLSupportMethod if
      -- you specified true for CloudFrontDefaultCertificate.
    } deriving (Show, Generic)

-- | If you want viewers to use HTTPS to request your objects and you're using
-- an alternate domain name in your object URLs (for example,
-- https://example.com/logo.jpg), specify the IAM certificate identifier of
-- the custom viewer certificate for this distribution. Specify either this
-- value or CloudFrontDefaultCertificate.
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId f x =
    f (_vcIAMCertificateId x)
        <&> \y -> x { _vcIAMCertificateId = y }
{-# INLINE vcIAMCertificateId #-}

-- | If you want viewers to use HTTPS to request your objects and you're using
-- the CloudFront domain name of your distribution in your object URLs (for
-- example, https://d111111abcdef8.cloudfront.net/logo.jpg), set to true. Omit
-- this value if you are setting an IAMCertificateId.
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate f x =
    f (_vcCloudFrontDefaultCertificate x)
        <&> \y -> x { _vcCloudFrontDefaultCertificate = y }
{-# INLINE vcCloudFrontDefaultCertificate #-}

-- | If you specify a value for IAMCertificateId, you must also specify how you
-- want CloudFront to serve HTTPS requests. Valid values are vip and sni-only.
-- If you specify vip, CloudFront uses dedicated IP addresses for your content
-- and can respond to HTTPS requests from any viewer. However, you must
-- request permission to use this feature, and you incur additional monthly
-- charges. If you specify sni-only, CloudFront can only respond to HTTPS
-- requests from viewers that support Server Name Indication (SNI). All modern
-- browsers support SNI, but some browsers still in use don't support SNI. Do
-- not specify a value for SSLSupportMethod if you specified true for
-- CloudFrontDefaultCertificate.
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod f x =
    f (_vcSSLSupportMethod x)
        <&> \y -> x { _vcSSLSupportMethod = y }
{-# INLINE vcSSLSupportMethod #-}

instance FromXML ViewerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ViewerCertificate"

instance ToXML ViewerCertificate where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ViewerCertificate"
