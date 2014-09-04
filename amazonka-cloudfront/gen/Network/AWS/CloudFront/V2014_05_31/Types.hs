{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , Restrictions
    , mkRestrictions
    , rsGeoRestriction

    -- * S3OriginConfig
    , S3OriginConfig
    , mkS3OriginConfig
    , socOriginAccessIdentity

    -- * ActiveTrustedSigners
    , ActiveTrustedSigners
    , atsEnabled
    , atsQuantity
    , atsItems

    -- * Aliases
    , Aliases
    , mkAliases
    , aQuantity
    , aItems

    -- * AllowedMethods
    , AllowedMethods
    , mkAllowedMethods
    , amQuantity
    , amItems

    -- * CacheBehavior
    , CacheBehavior
    , mkCacheBehavior
    , ccPathPattern
    , ccTargetOriginId
    , ccForwardedValues
    , ccTrustedSigners
    , ccViewerProtocolPolicy
    , ccMinTTL
    , ccAllowedMethods
    , ccSmoothStreaming

    -- * CacheBehaviors
    , CacheBehaviors
    , mkCacheBehaviors
    , cbQuantity
    , cbItems

    -- * CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity
    , cfoaiId
    , cfoaiS3CanonicalUserId
    , cfoaiCloudFrontOriginAccessIdentityConfig

    -- * CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig
    , mkCloudFrontOriginAccessIdentityConfig
    , cfoaicCallerReference
    , cfoaicComment

    -- * CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList
    , cfoailMarker
    , cfoailNextMarker
    , cfoailMaxItems
    , cfoailIsTruncated
    , cfoailQuantity
    , cfoailItems

    -- * CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary
    , cfoaisId
    , cfoaisS3CanonicalUserId
    , cfoaisComment

    -- * CookieNames
    , CookieNames
    , mkCookieNames
    , cnQuantity
    , cnItems

    -- * CookiePreference
    , CookiePreference
    , mkCookiePreference
    , cpForward
    , cpWhitelistedNames

    -- * CustomErrorResponse
    , CustomErrorResponse
    , mkCustomErrorResponse
    , cesErrorCode
    , cesResponsePagePath
    , cesResponseCode
    , cesErrorCachingMinTTL

    -- * CustomErrorResponses
    , CustomErrorResponses
    , mkCustomErrorResponses
    , cerQuantity
    , cerItems

    -- * CustomOriginConfig
    , CustomOriginConfig
    , mkCustomOriginConfig
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- * DefaultCacheBehavior
    , DefaultCacheBehavior
    , mkDefaultCacheBehavior
    , dcbTargetOriginId
    , dcbForwardedValues
    , dcbTrustedSigners
    , dcbViewerProtocolPolicy
    , dcbMinTTL
    , dcbAllowedMethods
    , dcbSmoothStreaming

    -- * Distribution
    , Distribution
    , dnId
    , dnStatus
    , dnLastModifiedTime
    , dnInProgressInvalidationBatches
    , dnDomainName
    , dnActiveTrustedSigners
    , dnDistributionConfig

    -- * DistributionConfig
    , DistributionConfig
    , mkDistributionConfig
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
    , DistributionList
    , dlMarker
    , dlNextMarker
    , dlMaxItems
    , dlIsTruncated
    , dlQuantity
    , dlItems

    -- * DistributionSummary
    , DistributionSummary
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
    , ForwardedValues
    , mkForwardedValues
    , fvQueryString
    , fvCookies
    , fvHeaders

    -- * GeoRestriction
    , GeoRestriction
    , mkGeoRestriction
    , grRestrictionType
    , grQuantity
    , grItems

    -- * Headers
    , Headers
    , mkHeaders
    , hQuantity
    , hItems

    -- * Invalidation
    , Invalidation
    , inId
    , inStatus
    , inCreateTime
    , inInvalidationBatch

    -- * InvalidationBatch
    , InvalidationBatch
    , mkInvalidationBatch
    , ibPaths
    , ibCallerReference

    -- * InvalidationList
    , InvalidationList
    , ilMarker
    , ilNextMarker
    , ilMaxItems
    , ilIsTruncated
    , ilQuantity
    , ilItems

    -- * InvalidationSummary
    , InvalidationSummary
    , iiiiiiiiiiiiyId
    , iiiiiiiiiiiiyCreateTime
    , iiiiiiiiiiiiyStatus

    -- * KeyPairIds
    , KeyPairIds
    , mkKeyPairIds
    , kpiQuantity
    , kpiItems

    -- * LoggingConfig
    , LoggingConfig
    , mkLoggingConfig
    , lcEnabled
    , lcIncludeCookies
    , lcBucket
    , lcPrefix

    -- * Origin
    , Origin
    , mkOrigin
    , pId
    , pDomainName
    , pS3OriginConfig
    , pCustomOriginConfig

    -- * Origins
    , Origins
    , mkOrigins
    , oQuantity
    , oItems

    -- * Paths
    , Paths
    , mkPaths
    , psQuantity
    , psItems

    -- * S3Origin
    , S3Origin
    , mkS3Origin
    , ssssnDomainName
    , ssssnOriginAccessIdentity

    -- * Signer
    , Signer
    , ssrAwsAccountNumber
    , ssrKeyPairIds

    -- * StreamingDistribution
    , StreamingDistribution
    , sdId
    , sdStatus
    , sdLastModifiedTime
    , sdDomainName
    , sdActiveTrustedSigners
    , sdStreamingDistributionConfig

    -- * StreamingDistributionConfig
    , StreamingDistributionConfig
    , mkStreamingDistributionConfig
    , sdcCallerReference
    , sdcS3Origin
    , sdcAliases
    , sdcComment
    , sdcLogging
    , sdcTrustedSigners
    , sdcPriceClass
    , sdcEnabled

    -- * StreamingDistributionList
    , StreamingDistributionList
    , sdlMarker
    , sdlNextMarker
    , sdlMaxItems
    , sdlIsTruncated
    , sdlQuantity
    , sdlItems

    -- * StreamingDistributionSummary
    , StreamingDistributionSummary
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
    , StreamingLoggingConfig
    , mkStreamingLoggingConfig
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- * TrustedSigners
    , TrustedSigners
    , mkTrustedSigners
    , tsEnabled
    , tsQuantity
    , tsItems

    -- * ViewerCertificate
    , ViewerCertificate
    , mkViewerCertificate
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
rsGeoRestriction = lens _rsGeoRestriction (\s a -> s { _rsGeoRestriction = a })
{-# INLINE rsGeoRestriction #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Restrictions' data type to populate a request.
mkRestrictions :: GeoRestriction -- ^ 'rsGeoRestriction'
               -> Restrictions
mkRestrictions p1 = Restrictions
    { _rsGeoRestriction = p1
    }
{-# INLINE mkRestrictions #-}

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
socOriginAccessIdentity = lens _socOriginAccessIdentity (\s a -> s { _socOriginAccessIdentity = a })
{-# INLINE socOriginAccessIdentity #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3OriginConfig' data type to populate a request.
mkS3OriginConfig :: Text -- ^ 'socOriginAccessIdentity'
                 -> S3OriginConfig
mkS3OriginConfig p1 = S3OriginConfig
    { _socOriginAccessIdentity = p1
    }
{-# INLINE mkS3OriginConfig #-}

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
atsEnabled = lens _atsEnabled (\s a -> s { _atsEnabled = a })
{-# INLINE atsEnabled #-}

-- | The number of unique trusted signers included in all cache behaviors. For
-- example, if three cache behaviors all list the same three AWS accounts, the
-- value of Quantity for ActiveTrustedSigners will be 3.
atsQuantity :: Lens' ActiveTrustedSigners (Integer)
atsQuantity = lens _atsQuantity (\s a -> s { _atsQuantity = a })
{-# INLINE atsQuantity #-}

-- | A complex type that contains one Signer complex type for each unique
-- trusted signer that is specified in the TrustedSigners complex type,
-- including trusted signers in the default cache behavior and in all of the
-- other cache behaviors.
atsItems :: Lens' ActiveTrustedSigners ([Signer])
atsItems = lens _atsItems (\s a -> s { _atsItems = a })
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
aQuantity = lens _aQuantity (\s a -> s { _aQuantity = a })
{-# INLINE aQuantity #-}

-- | Optional: A complex type that contains CNAME elements, if any, for this
-- distribution. If Quantity is 0, you can omit Items.
aItems :: Lens' Aliases ([Text])
aItems = lens _aItems (\s a -> s { _aItems = a })
{-# INLINE aItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Aliases' data type to populate a request.
mkAliases :: Integer -- ^ 'aQuantity'
          -> Aliases
mkAliases p1 = Aliases
    { _aQuantity = p1
    , _aItems = mempty
    }
{-# INLINE mkAliases #-}

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
amQuantity = lens _amQuantity (\s a -> s { _amQuantity = a })
{-# INLINE amQuantity #-}

-- | A complex type that contains the HTTP methods that you want CloudFront to
-- process and forward to your origin.
amItems :: Lens' AllowedMethods ([Method])
amItems = lens _amItems (\s a -> s { _amItems = a })
{-# INLINE amItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AllowedMethods' data type to populate a request.
mkAllowedMethods :: Integer -- ^ 'amQuantity'
                 -> AllowedMethods
mkAllowedMethods p1 = AllowedMethods
    { _amQuantity = p1
    , _amItems = mempty
    }
{-# INLINE mkAllowedMethods #-}

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
ccPathPattern = lens _ccPathPattern (\s a -> s { _ccPathPattern = a })
{-# INLINE ccPathPattern #-}

-- | The value of ID for the origin that you want CloudFront to route requests
-- to when a request matches the path pattern either for a cache behavior or
-- for the default cache behavior.
ccTargetOriginId :: Lens' CacheBehavior (Text)
ccTargetOriginId = lens _ccTargetOriginId (\s a -> s { _ccTargetOriginId = a })
{-# INLINE ccTargetOriginId #-}

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
ccForwardedValues :: Lens' CacheBehavior (ForwardedValues)
ccForwardedValues = lens _ccForwardedValues (\s a -> s { _ccForwardedValues = a })
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
ccTrustedSigners = lens _ccTrustedSigners (\s a -> s { _ccTrustedSigners = a })
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
ccViewerProtocolPolicy = lens _ccViewerProtocolPolicy (\s a -> s { _ccViewerProtocolPolicy = a })
{-# INLINE ccViewerProtocolPolicy #-}

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
-- years).
ccMinTTL :: Lens' CacheBehavior (Integer)
ccMinTTL = lens _ccMinTTL (\s a -> s { _ccMinTTL = a })
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
ccAllowedMethods = lens _ccAllowedMethods (\s a -> s { _ccAllowedMethods = a })
{-# INLINE ccAllowedMethods #-}

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
ccSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
ccSmoothStreaming = lens _ccSmoothStreaming (\s a -> s { _ccSmoothStreaming = a })
{-# INLINE ccSmoothStreaming #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheBehavior' data type to populate a request.
mkCacheBehavior :: Text -- ^ 'ccPathPattern'
                -> Text -- ^ 'ccTargetOriginId'
                -> ForwardedValues -- ^ 'ccForwardedValues'
                -> TrustedSigners -- ^ 'ccTrustedSigners'
                -> ViewerProtocolPolicy -- ^ 'ccViewerProtocolPolicy'
                -> Integer -- ^ 'ccMinTTL'
                -> CacheBehavior
mkCacheBehavior p1 p2 p3 p4 p5 p6 = CacheBehavior
    { _ccPathPattern = p1
    , _ccTargetOriginId = p2
    , _ccForwardedValues = p3
    , _ccTrustedSigners = p4
    , _ccViewerProtocolPolicy = p5
    , _ccMinTTL = p6
    , _ccAllowedMethods = Nothing
    , _ccSmoothStreaming = Nothing
    }
{-# INLINE mkCacheBehavior #-}

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
cbQuantity = lens _cbQuantity (\s a -> s { _cbQuantity = a })
{-# INLINE cbQuantity #-}

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If Quantity is 0, you can omit Items.
cbItems :: Lens' CacheBehaviors ([CacheBehavior])
cbItems = lens _cbItems (\s a -> s { _cbItems = a })
{-# INLINE cbItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheBehaviors' data type to populate a request.
mkCacheBehaviors :: Integer -- ^ 'cbQuantity'
                 -> CacheBehaviors
mkCacheBehaviors p1 = CacheBehaviors
    { _cbQuantity = p1
    , _cbItems = mempty
    }
{-# INLINE mkCacheBehaviors #-}

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
cfoaiId = lens _cfoaiId (\s a -> s { _cfoaiId = a })
{-# INLINE cfoaiId #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you
-- use when giving the origin access identity read permission to an object in
-- Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity (Text)
cfoaiS3CanonicalUserId = lens _cfoaiS3CanonicalUserId (\s a -> s { _cfoaiS3CanonicalUserId = a })
{-# INLINE cfoaiS3CanonicalUserId #-}

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = lens _cfoaiCloudFrontOriginAccessIdentityConfig (\s a -> s { _cfoaiCloudFrontOriginAccessIdentityConfig = a })
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
cfoaicCallerReference = lens _cfoaicCallerReference (\s a -> s { _cfoaicCallerReference = a })
{-# INLINE cfoaicCallerReference #-}

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig (Text)
cfoaicComment = lens _cfoaicComment (\s a -> s { _cfoaicComment = a })
{-# INLINE cfoaicComment #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CloudFrontOriginAccessIdentityConfig' data type to populate a request.
mkCloudFrontOriginAccessIdentityConfig :: Text -- ^ 'cfoaicCallerReference'
                                       -> Text -- ^ 'cfoaicComment'
                                       -> CloudFrontOriginAccessIdentityConfig
mkCloudFrontOriginAccessIdentityConfig p1 p2 = CloudFrontOriginAccessIdentityConfig
    { _cfoaicCallerReference = p1
    , _cfoaicComment = p2
    }
{-# INLINE mkCloudFrontOriginAccessIdentityConfig #-}

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
cfoailMarker = lens _cfoailMarker (\s a -> s { _cfoailMarker = a })
{-# INLINE cfoailMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your origin
-- access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\s a -> s { _cfoailNextMarker = a })
{-# INLINE cfoailNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList (Integer)
cfoailMaxItems = lens _cfoailMaxItems (\s a -> s { _cfoailMaxItems = a })
{-# INLINE cfoailMaxItems #-}

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more items in the
-- list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList (Bool)
cfoailIsTruncated = lens _cfoailIsTruncated (\s a -> s { _cfoailIsTruncated = a })
{-# INLINE cfoailIsTruncated #-}

-- | The number of CloudFront origin access identities that were created by the
-- current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList (Integer)
cfoailQuantity = lens _cfoailQuantity (\s a -> s { _cfoailQuantity = a })
{-# INLINE cfoailQuantity #-}

-- | A complex type that contains one CloudFrontOriginAccessIdentitySummary
-- element for each origin access identity that was created by the current AWS
-- account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList ([CloudFrontOriginAccessIdentitySummary])
cfoailItems = lens _cfoailItems (\s a -> s { _cfoailItems = a })
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
cfoaisId = lens _cfoaisId (\s a -> s { _cfoaisId = a })
{-# INLINE cfoaisId #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you
-- use when giving the origin access identity read permission to an object in
-- Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary (Text)
cfoaisS3CanonicalUserId = lens _cfoaisS3CanonicalUserId (\s a -> s { _cfoaisS3CanonicalUserId = a })
{-# INLINE cfoaisS3CanonicalUserId #-}

-- | The comment for this origin access identity, as originally specified when
-- created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary (Text)
cfoaisComment = lens _cfoaisComment (\s a -> s { _cfoaisComment = a })
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
cnQuantity = lens _cnQuantity (\s a -> s { _cnQuantity = a })
{-# INLINE cnQuantity #-}

-- | Optional: A complex type that contains whitelisted cookies for this cache
-- behavior. If Quantity is 0, you can omit Items.
cnItems :: Lens' CookieNames ([Text])
cnItems = lens _cnItems (\s a -> s { _cnItems = a })
{-# INLINE cnItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CookieNames' data type to populate a request.
mkCookieNames :: Integer -- ^ 'cnQuantity'
              -> CookieNames
mkCookieNames p1 = CookieNames
    { _cnQuantity = p1
    , _cnItems = mempty
    }
{-# INLINE mkCookieNames #-}

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
cpForward = lens _cpForward (\s a -> s { _cpForward = a })
{-# INLINE cpForward #-}

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames = lens _cpWhitelistedNames (\s a -> s { _cpWhitelistedNames = a })
{-# INLINE cpWhitelistedNames #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CookiePreference' data type to populate a request.
mkCookiePreference :: ItemSelection -- ^ 'cpForward'
                   -> CookiePreference
mkCookiePreference p1 = CookiePreference
    { _cpForward = p1
    , _cpWhitelistedNames = Nothing
    }
{-# INLINE mkCookiePreference #-}

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
cesErrorCode = lens _cesErrorCode (\s a -> s { _cesErrorCode = a })
{-# INLINE cesErrorCode #-}

-- | The path of the custom error page (for example, /custom_404.html). The path
-- is relative to the distribution and must begin with a slash (/). If the
-- path includes any non-ASCII characters or unsafe characters as defined in
-- RFC 1783 (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
-- characters. Do not URL encode any other characters in the path, or
-- CloudFront will not return the custom error page to the viewer.
cesResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
cesResponsePagePath = lens _cesResponsePagePath (\s a -> s { _cesResponsePagePath = a })
{-# INLINE cesResponsePagePath #-}

-- | The HTTP status code that you want CloudFront to return with the custom
-- error page to the viewer. For a list of HTTP status codes that you can
-- replace, see CloudFront Documentation.
cesResponseCode :: Lens' CustomErrorResponse (Maybe Text)
cesResponseCode = lens _cesResponseCode (\s a -> s { _cesResponseCode = a })
{-# INLINE cesResponseCode #-}

-- | The minimum amount of time you want HTTP error codes to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated. You can specify a value from 0 to 31,536,000.
cesErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
cesErrorCachingMinTTL = lens _cesErrorCachingMinTTL (\s a -> s { _cesErrorCachingMinTTL = a })
{-# INLINE cesErrorCachingMinTTL #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CustomErrorResponse' data type to populate a request.
mkCustomErrorResponse :: Integer -- ^ 'cesErrorCode'
                      -> CustomErrorResponse
mkCustomErrorResponse p1 = CustomErrorResponse
    { _cesErrorCode = p1
    , _cesResponsePagePath = Nothing
    , _cesResponseCode = Nothing
    , _cesErrorCachingMinTTL = Nothing
    }
{-# INLINE mkCustomErrorResponse #-}

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
cerQuantity = lens _cerQuantity (\s a -> s { _cerQuantity = a })
{-# INLINE cerQuantity #-}

-- | Optional: A complex type that contains custom error responses for this
-- distribution. If Quantity is 0, you can omit Items.
cerItems :: Lens' CustomErrorResponses ([CustomErrorResponse])
cerItems = lens _cerItems (\s a -> s { _cerItems = a })
{-# INLINE cerItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CustomErrorResponses' data type to populate a request.
mkCustomErrorResponses :: Integer -- ^ 'cerQuantity'
                       -> CustomErrorResponses
mkCustomErrorResponses p1 = CustomErrorResponses
    { _cerQuantity = p1
    , _cerItems = mempty
    }
{-# INLINE mkCustomErrorResponses #-}

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
cocHTTPPort = lens _cocHTTPPort (\s a -> s { _cocHTTPPort = a })
{-# INLINE cocHTTPPort #-}

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig (Integer)
cocHTTPSPort = lens _cocHTTPSPort (\s a -> s { _cocHTTPSPort = a })
{-# INLINE cocHTTPSPort #-}

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig (OriginProtocolPolicy)
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\s a -> s { _cocOriginProtocolPolicy = a })
{-# INLINE cocOriginProtocolPolicy #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CustomOriginConfig' data type to populate a request.
mkCustomOriginConfig :: Integer -- ^ 'cocHTTPPort'
                     -> Integer -- ^ 'cocHTTPSPort'
                     -> OriginProtocolPolicy -- ^ 'cocOriginProtocolPolicy'
                     -> CustomOriginConfig
mkCustomOriginConfig p1 p2 p3 = CustomOriginConfig
    { _cocHTTPPort = p1
    , _cocHTTPSPort = p2
    , _cocOriginProtocolPolicy = p3
    }
{-# INLINE mkCustomOriginConfig #-}

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
dcbTargetOriginId = lens _dcbTargetOriginId (\s a -> s { _dcbTargetOriginId = a })
{-# INLINE dcbTargetOriginId #-}

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior (ForwardedValues)
dcbForwardedValues = lens _dcbForwardedValues (\s a -> s { _dcbForwardedValues = a })
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
dcbTrustedSigners = lens _dcbTrustedSigners (\s a -> s { _dcbTrustedSigners = a })
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
dcbViewerProtocolPolicy = lens _dcbViewerProtocolPolicy (\s a -> s { _dcbViewerProtocolPolicy = a })
{-# INLINE dcbViewerProtocolPolicy #-}

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated.You can specify a value from 0 to 3,153,600,000 seconds (100
-- years).
dcbMinTTL :: Lens' DefaultCacheBehavior (Integer)
dcbMinTTL = lens _dcbMinTTL (\s a -> s { _dcbMinTTL = a })
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
dcbAllowedMethods = lens _dcbAllowedMethods (\s a -> s { _dcbAllowedMethods = a })
{-# INLINE dcbAllowedMethods #-}

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming = lens _dcbSmoothStreaming (\s a -> s { _dcbSmoothStreaming = a })
{-# INLINE dcbSmoothStreaming #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DefaultCacheBehavior' data type to populate a request.
mkDefaultCacheBehavior :: Text -- ^ 'dcbTargetOriginId'
                       -> ForwardedValues -- ^ 'dcbForwardedValues'
                       -> TrustedSigners -- ^ 'dcbTrustedSigners'
                       -> ViewerProtocolPolicy -- ^ 'dcbViewerProtocolPolicy'
                       -> Integer -- ^ 'dcbMinTTL'
                       -> DefaultCacheBehavior
mkDefaultCacheBehavior p1 p2 p3 p4 p5 = DefaultCacheBehavior
    { _dcbTargetOriginId = p1
    , _dcbForwardedValues = p2
    , _dcbTrustedSigners = p3
    , _dcbViewerProtocolPolicy = p4
    , _dcbMinTTL = p5
    , _dcbAllowedMethods = Nothing
    , _dcbSmoothStreaming = Nothing
    }
{-# INLINE mkDefaultCacheBehavior #-}

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
dnId = lens _dnId (\s a -> s { _dnId = a })
{-# INLINE dnId #-}

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution's information is fully
-- propagated throughout the Amazon CloudFront system.
dnStatus :: Lens' Distribution (Text)
dnStatus = lens _dnStatus (\s a -> s { _dnStatus = a })
{-# INLINE dnStatus #-}

-- | The date and time the distribution was last modified.
dnLastModifiedTime :: Lens' Distribution (ISO8601)
dnLastModifiedTime = lens _dnLastModifiedTime (\s a -> s { _dnLastModifiedTime = a })
{-# INLINE dnLastModifiedTime #-}

-- | The number of invalidation batches currently in progress.
dnInProgressInvalidationBatches :: Lens' Distribution (Integer)
dnInProgressInvalidationBatches = lens _dnInProgressInvalidationBatches (\s a -> s { _dnInProgressInvalidationBatches = a })
{-# INLINE dnInProgressInvalidationBatches #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dnDomainName :: Lens' Distribution (Text)
dnDomainName = lens _dnDomainName (\s a -> s { _dnDomainName = a })
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
dnActiveTrustedSigners = lens _dnActiveTrustedSigners (\s a -> s { _dnActiveTrustedSigners = a })
{-# INLINE dnActiveTrustedSigners #-}

-- | The current configuration information for the distribution.
dnDistributionConfig :: Lens' Distribution (DistributionConfig)
dnDistributionConfig = lens _dnDistributionConfig (\s a -> s { _dnDistributionConfig = a })
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
dcCallerReference = lens _dcCallerReference (\s a -> s { _dcCallerReference = a })
{-# INLINE dcCallerReference #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Aliases)
dcAliases = lens _dcAliases (\s a -> s { _dcAliases = a })
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
dcDefaultRootObject = lens _dcDefaultRootObject (\s a -> s { _dcDefaultRootObject = a })
{-# INLINE dcDefaultRootObject #-}

-- | A complex type that contains information about origins for this
-- distribution.
dcOrigins :: Lens' DistributionConfig (Origins)
dcOrigins = lens _dcOrigins (\s a -> s { _dcOrigins = a })
{-# INLINE dcOrigins #-}

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig (DefaultCacheBehavior)
dcDefaultCacheBehavior = lens _dcDefaultCacheBehavior (\s a -> s { _dcDefaultCacheBehavior = a })
{-# INLINE dcDefaultCacheBehavior #-}

-- | A complex type that contains zero or more CacheBehavior elements.
dcCacheBehaviors :: Lens' DistributionConfig (CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\s a -> s { _dcCacheBehaviors = a })
{-# INLINE dcCacheBehaviors #-}

-- | A complex type that contains zero or more CustomErrorResponse elements.
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses = lens _dcCustomErrorResponses (\s a -> s { _dcCustomErrorResponses = a })
{-# INLINE dcCustomErrorResponses #-}

-- | Any comments you want to include about the distribution.
dcComment :: Lens' DistributionConfig (Text)
dcComment = lens _dcComment (\s a -> s { _dcComment = a })
{-# INLINE dcComment #-}

-- | A complex type that controls whether access logs are written for the
-- distribution.
dcLogging :: Lens' DistributionConfig (LoggingConfig)
dcLogging = lens _dcLogging (\s a -> s { _dcLogging = a })
{-# INLINE dcLogging #-}

-- | A complex type that contains information about price class for this
-- distribution.
dcPriceClass :: Lens' DistributionConfig (PriceClass)
dcPriceClass = lens _dcPriceClass (\s a -> s { _dcPriceClass = a })
{-# INLINE dcPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dcEnabled :: Lens' DistributionConfig (Bool)
dcEnabled = lens _dcEnabled (\s a -> s { _dcEnabled = a })
{-# INLINE dcEnabled #-}

-- | A complex type that contains information about viewer certificates for this
-- distribution.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate = lens _dcViewerCertificate (\s a -> s { _dcViewerCertificate = a })
{-# INLINE dcViewerCertificate #-}

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\s a -> s { _dcRestrictions = a })
{-# INLINE dcRestrictions #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DistributionConfig' data type to populate a request.
mkDistributionConfig :: Text -- ^ 'dcCallerReference'
                     -> Aliases -- ^ 'dcAliases'
                     -> Text -- ^ 'dcDefaultRootObject'
                     -> Origins -- ^ 'dcOrigins'
                     -> DefaultCacheBehavior -- ^ 'dcDefaultCacheBehavior'
                     -> CacheBehaviors -- ^ 'dcCacheBehaviors'
                     -> Text -- ^ 'dcComment'
                     -> LoggingConfig -- ^ 'dcLogging'
                     -> PriceClass -- ^ 'dcPriceClass'
                     -> Bool -- ^ 'dcEnabled'
                     -> DistributionConfig
mkDistributionConfig p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 = DistributionConfig
    { _dcCallerReference = p1
    , _dcAliases = p2
    , _dcDefaultRootObject = p3
    , _dcOrigins = p4
    , _dcDefaultCacheBehavior = p5
    , _dcCacheBehaviors = p6
    , _dcCustomErrorResponses = Nothing
    , _dcComment = p8
    , _dcLogging = p9
    , _dcPriceClass = p10
    , _dcEnabled = p11
    , _dcViewerCertificate = Nothing
    , _dcRestrictions = Nothing
    }
{-# INLINE mkDistributionConfig #-}

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
dlMarker = lens _dlMarker (\s a -> s { _dlMarker = a })
{-# INLINE dlMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\s a -> s { _dlNextMarker = a })
{-# INLINE dlNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
dlMaxItems :: Lens' DistributionList (Integer)
dlMaxItems = lens _dlMaxItems (\s a -> s { _dlMaxItems = a })
{-# INLINE dlMaxItems #-}

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the Marker request parameter to retrieve more distributions in the
-- list.
dlIsTruncated :: Lens' DistributionList (Bool)
dlIsTruncated = lens _dlIsTruncated (\s a -> s { _dlIsTruncated = a })
{-# INLINE dlIsTruncated #-}

-- | The number of distributions that were created by the current AWS account.
dlQuantity :: Lens' DistributionList (Integer)
dlQuantity = lens _dlQuantity (\s a -> s { _dlQuantity = a })
{-# INLINE dlQuantity #-}

-- | A complex type that contains one DistributionSummary element for each
-- distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList ([DistributionSummary])
dlItems = lens _dlItems (\s a -> s { _dlItems = a })
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
dsId = lens _dsId (\s a -> s { _dsId = a })
{-# INLINE dsId #-}

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution's information is fully
-- propagated throughout the Amazon CloudFront system.
dsStatus :: Lens' DistributionSummary (Text)
dsStatus = lens _dsStatus (\s a -> s { _dsStatus = a })
{-# INLINE dsStatus #-}

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary (ISO8601)
dsLastModifiedTime = lens _dsLastModifiedTime (\s a -> s { _dsLastModifiedTime = a })
{-# INLINE dsLastModifiedTime #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dsDomainName :: Lens' DistributionSummary (Text)
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })
{-# INLINE dsDomainName #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary (Aliases)
dsAliases = lens _dsAliases (\s a -> s { _dsAliases = a })
{-# INLINE dsAliases #-}

-- | A complex type that contains information about origins for this
-- distribution.
dsOrigins :: Lens' DistributionSummary (Origins)
dsOrigins = lens _dsOrigins (\s a -> s { _dsOrigins = a })
{-# INLINE dsOrigins #-}

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values
-- of PathPattern in CacheBehavior elements.You must create exactly one
-- default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary (DefaultCacheBehavior)
dsDefaultCacheBehavior = lens _dsDefaultCacheBehavior (\s a -> s { _dsDefaultCacheBehavior = a })
{-# INLINE dsDefaultCacheBehavior #-}

-- | A complex type that contains zero or more CacheBehavior elements.
dsCacheBehaviors :: Lens' DistributionSummary (CacheBehaviors)
dsCacheBehaviors = lens _dsCacheBehaviors (\s a -> s { _dsCacheBehaviors = a })
{-# INLINE dsCacheBehaviors #-}

-- | A complex type that contains zero or more CustomErrorResponses elements.
dsCustomErrorResponses :: Lens' DistributionSummary (CustomErrorResponses)
dsCustomErrorResponses = lens _dsCustomErrorResponses (\s a -> s { _dsCustomErrorResponses = a })
{-# INLINE dsCustomErrorResponses #-}

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary (Text)
dsComment = lens _dsComment (\s a -> s { _dsComment = a })
{-# INLINE dsComment #-}

dsPriceClass :: Lens' DistributionSummary (PriceClass)
dsPriceClass = lens _dsPriceClass (\s a -> s { _dsPriceClass = a })
{-# INLINE dsPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dsEnabled :: Lens' DistributionSummary (Bool)
dsEnabled = lens _dsEnabled (\s a -> s { _dsEnabled = a })
{-# INLINE dsEnabled #-}

-- | A complex type that contains information about viewer certificates for this
-- distribution.
dsViewerCertificate :: Lens' DistributionSummary (ViewerCertificate)
dsViewerCertificate = lens _dsViewerCertificate (\s a -> s { _dsViewerCertificate = a })
{-# INLINE dsViewerCertificate #-}

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
dsRestrictions :: Lens' DistributionSummary (Restrictions)
dsRestrictions = lens _dsRestrictions (\s a -> s { _dsRestrictions = a })
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
fvQueryString = lens _fvQueryString (\s a -> s { _fvQueryString = a })
{-# INLINE fvQueryString #-}

-- | A complex type that specifies how CloudFront handles cookies.
fvCookies :: Lens' ForwardedValues (CookiePreference)
fvCookies = lens _fvCookies (\s a -> s { _fvCookies = a })
{-# INLINE fvCookies #-}

-- | A complex type that specifies the Headers, if any, that you want CloudFront
-- to vary upon for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\s a -> s { _fvHeaders = a })
{-# INLINE fvHeaders #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ForwardedValues' data type to populate a request.
mkForwardedValues :: Bool -- ^ 'fvQueryString'
                  -> CookiePreference -- ^ 'fvCookies'
                  -> ForwardedValues
mkForwardedValues p1 p2 = ForwardedValues
    { _fvQueryString = p1
    , _fvCookies = p2
    , _fvHeaders = Nothing
    }
{-# INLINE mkForwardedValues #-}

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
grRestrictionType = lens _grRestrictionType (\s a -> s { _grRestrictionType = a })
{-# INLINE grRestrictionType #-}

-- | When geo restriction is enabled, this is the number of countries in your
-- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is 0,
-- and you can omit Items.
grQuantity :: Lens' GeoRestriction (Integer)
grQuantity = lens _grQuantity (\s a -> s { _grQuantity = a })
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
grItems = lens _grItems (\s a -> s { _grItems = a })
{-# INLINE grItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GeoRestriction' data type to populate a request.
mkGeoRestriction :: GeoRestrictionType -- ^ 'grRestrictionType'
                 -> Integer -- ^ 'grQuantity'
                 -> GeoRestriction
mkGeoRestriction p1 p2 = GeoRestriction
    { _grRestrictionType = p1
    , _grQuantity = p2
    , _grItems = mempty
    }
{-# INLINE mkGeoRestriction #-}

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
hQuantity = lens _hQuantity (\s a -> s { _hQuantity = a })
{-# INLINE hQuantity #-}

-- | Optional: A complex type that contains a Name element for each header that
-- you want CloudFront to forward to the origin and to vary on for this cache
-- behavior. If Quantity is 0, omit Items.
hItems :: Lens' Headers ([Text])
hItems = lens _hItems (\s a -> s { _hItems = a })
{-# INLINE hItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Headers' data type to populate a request.
mkHeaders :: Integer -- ^ 'hQuantity'
          -> Headers
mkHeaders p1 = Headers
    { _hQuantity = p1
    , _hItems = mempty
    }
{-# INLINE mkHeaders #-}

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
inId = lens _inId (\s a -> s { _inId = a })
{-# INLINE inId #-}

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is Completed.
inStatus :: Lens' Invalidation (Text)
inStatus = lens _inStatus (\s a -> s { _inStatus = a })
{-# INLINE inStatus #-}

-- | The date and time the invalidation request was first made.
inCreateTime :: Lens' Invalidation (ISO8601)
inCreateTime = lens _inCreateTime (\s a -> s { _inCreateTime = a })
{-# INLINE inCreateTime #-}

-- | The current invalidation information for the batch request.
inInvalidationBatch :: Lens' Invalidation (InvalidationBatch)
inInvalidationBatch = lens _inInvalidationBatch (\s a -> s { _inInvalidationBatch = a })
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
ibPaths = lens _ibPaths (\s a -> s { _ibPaths = a })
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
ibCallerReference = lens _ibCallerReference (\s a -> s { _ibCallerReference = a })
{-# INLINE ibCallerReference #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InvalidationBatch' data type to populate a request.
mkInvalidationBatch :: Paths -- ^ 'ibPaths'
                    -> Text -- ^ 'ibCallerReference'
                    -> InvalidationBatch
mkInvalidationBatch p1 p2 = InvalidationBatch
    { _ibPaths = p1
    , _ibCallerReference = p2
    }
{-# INLINE mkInvalidationBatch #-}

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
ilMarker = lens _ilMarker (\s a -> s { _ilMarker = a })
{-# INLINE ilMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\s a -> s { _ilNextMarker = a })
{-# INLINE ilNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
ilMaxItems :: Lens' InvalidationList (Integer)
ilMaxItems = lens _ilMaxItems (\s a -> s { _ilMaxItems = a })
{-# INLINE ilMaxItems #-}

-- | A flag that indicates whether more invalidation batch requests remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more invalidation
-- batches in the list.
ilIsTruncated :: Lens' InvalidationList (Bool)
ilIsTruncated = lens _ilIsTruncated (\s a -> s { _ilIsTruncated = a })
{-# INLINE ilIsTruncated #-}

-- | The number of invalidation batches that were created by the current AWS
-- account.
ilQuantity :: Lens' InvalidationList (Integer)
ilQuantity = lens _ilQuantity (\s a -> s { _ilQuantity = a })
{-# INLINE ilQuantity #-}

-- | A complex type that contains one InvalidationSummary element for each
-- invalidation batch that was created by the current AWS account.
ilItems :: Lens' InvalidationList ([InvalidationSummary])
ilItems = lens _ilItems (\s a -> s { _ilItems = a })
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
iiiiiiiiiiiiyId = lens _iiiiiiiiiiiiyId (\s a -> s { _iiiiiiiiiiiiyId = a })
{-# INLINE iiiiiiiiiiiiyId #-}

iiiiiiiiiiiiyCreateTime :: Lens' InvalidationSummary (ISO8601)
iiiiiiiiiiiiyCreateTime = lens _iiiiiiiiiiiiyCreateTime (\s a -> s { _iiiiiiiiiiiiyCreateTime = a })
{-# INLINE iiiiiiiiiiiiyCreateTime #-}

-- | The status of an invalidation request.
iiiiiiiiiiiiyStatus :: Lens' InvalidationSummary (Text)
iiiiiiiiiiiiyStatus = lens _iiiiiiiiiiiiyStatus (\s a -> s { _iiiiiiiiiiiiyStatus = a })
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
kpiQuantity = lens _kpiQuantity (\s a -> s { _kpiQuantity = a })
{-# INLINE kpiQuantity #-}

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
kpiItems :: Lens' KeyPairIds ([Text])
kpiItems = lens _kpiItems (\s a -> s { _kpiItems = a })
{-# INLINE kpiItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'KeyPairIds' data type to populate a request.
mkKeyPairIds :: Integer -- ^ 'kpiQuantity'
             -> KeyPairIds
mkKeyPairIds p1 = KeyPairIds
    { _kpiQuantity = p1
    , _kpiItems = mempty
    }
{-# INLINE mkKeyPairIds #-}

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
lcEnabled = lens _lcEnabled (\s a -> s { _lcEnabled = a })
{-# INLINE lcEnabled #-}

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify true for IncludeCookies. If you choose to include cookies in logs,
-- CloudFront logs all cookies regardless of how you configure the cache
-- behaviors for this distribution. If you do not want to include cookies when
-- you create a distribution or if you want to disable include cookies for an
-- existing distribution, specify false for IncludeCookies.
lcIncludeCookies :: Lens' LoggingConfig (Bool)
lcIncludeCookies = lens _lcIncludeCookies (\s a -> s { _lcIncludeCookies = a })
{-# INLINE lcIncludeCookies #-}

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
lcBucket :: Lens' LoggingConfig (Text)
lcBucket = lens _lcBucket (\s a -> s { _lcBucket = a })
{-# INLINE lcBucket #-}

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this distribution, for example, myprefix/. If you want to
-- enable logging, but you do not want to specify a prefix, you still must
-- include an empty Prefix element in the Logging element.
lcPrefix :: Lens' LoggingConfig (Text)
lcPrefix = lens _lcPrefix (\s a -> s { _lcPrefix = a })
{-# INLINE lcPrefix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoggingConfig' data type to populate a request.
mkLoggingConfig :: Bool -- ^ 'lcEnabled'
                -> Bool -- ^ 'lcIncludeCookies'
                -> Text -- ^ 'lcBucket'
                -> Text -- ^ 'lcPrefix'
                -> LoggingConfig
mkLoggingConfig p1 p2 p3 p4 = LoggingConfig
    { _lcEnabled = p1
    , _lcIncludeCookies = p2
    , _lcBucket = p3
    , _lcPrefix = p4
    }
{-# INLINE mkLoggingConfig #-}

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
pId = lens _pId (\s a -> s { _pId = a })
{-# INLINE pId #-}

-- | Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you want
-- CloudFront to get objects for this origin, for example,
-- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for the
-- HTTP server from which you want CloudFront to get objects for this origin,
-- for example, www.example.com.
pDomainName :: Lens' Origin (Text)
pDomainName = lens _pDomainName (\s a -> s { _pDomainName = a })
{-# INLINE pDomainName #-}

-- | A complex type that contains information about the Amazon S3 origin. If the
-- origin is a custom origin, use the CustomOriginConfig element instead.
pS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
pS3OriginConfig = lens _pS3OriginConfig (\s a -> s { _pS3OriginConfig = a })
{-# INLINE pS3OriginConfig #-}

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
pCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
pCustomOriginConfig = lens _pCustomOriginConfig (\s a -> s { _pCustomOriginConfig = a })
{-# INLINE pCustomOriginConfig #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Origin' data type to populate a request.
mkOrigin :: Text -- ^ 'pId'
         -> Text -- ^ 'pDomainName'
         -> Origin
mkOrigin p1 p2 = Origin
    { _pId = p1
    , _pDomainName = p2
    , _pS3OriginConfig = Nothing
    , _pCustomOriginConfig = Nothing
    }
{-# INLINE mkOrigin #-}

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
oQuantity = lens _oQuantity (\s a -> s { _oQuantity = a })
{-# INLINE oQuantity #-}

-- | A complex type that contains origins for this distribution.
oItems :: Lens' Origins (Maybe [Origin])
oItems = lens _oItems (\s a -> s { _oItems = a })
{-# INLINE oItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Origins' data type to populate a request.
mkOrigins :: Integer -- ^ 'oQuantity'
          -> Origins
mkOrigins p1 = Origins
    { _oQuantity = p1
    , _oItems = Nothing
    }
{-# INLINE mkOrigins #-}

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
psQuantity = lens _psQuantity (\s a -> s { _psQuantity = a })
{-# INLINE psQuantity #-}

-- | A complex type that contains a list of the objects that you want to
-- invalidate.
psItems :: Lens' Paths ([Text])
psItems = lens _psItems (\s a -> s { _psItems = a })
{-# INLINE psItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Paths' data type to populate a request.
mkPaths :: Integer -- ^ 'psQuantity'
        -> Paths
mkPaths p1 = Paths
    { _psQuantity = p1
    , _psItems = mempty
    }
{-# INLINE mkPaths #-}

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
ssssnDomainName = lens _ssssnDomainName (\s a -> s { _ssssnDomainName = a })
{-# INLINE ssssnDomainName #-}

-- | Your S3 origin's origin access identity.
ssssnOriginAccessIdentity :: Lens' S3Origin (Text)
ssssnOriginAccessIdentity = lens _ssssnOriginAccessIdentity (\s a -> s { _ssssnOriginAccessIdentity = a })
{-# INLINE ssssnOriginAccessIdentity #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3Origin' data type to populate a request.
mkS3Origin :: Text -- ^ 'ssssnDomainName'
           -> Text -- ^ 'ssssnOriginAccessIdentity'
           -> S3Origin
mkS3Origin p1 p2 = S3Origin
    { _ssssnDomainName = p1
    , _ssssnOriginAccessIdentity = p2
    }
{-# INLINE mkS3Origin #-}

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
ssrAwsAccountNumber = lens _ssrAwsAccountNumber (\s a -> s { _ssrAwsAccountNumber = a })
{-# INLINE ssrAwsAccountNumber #-}

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
ssrKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
ssrKeyPairIds = lens _ssrKeyPairIds (\s a -> s { _ssrKeyPairIds = a })
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
sdId = lens _sdId (\s a -> s { _sdId = a })
{-# INLINE sdId #-}

-- | The current status of the streaming distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdStatus :: Lens' StreamingDistribution (Text)
sdStatus = lens _sdStatus (\s a -> s { _sdStatus = a })
{-# INLINE sdStatus #-}

-- | The date and time the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe ISO8601)
sdLastModifiedTime = lens _sdLastModifiedTime (\s a -> s { _sdLastModifiedTime = a })
{-# INLINE sdLastModifiedTime #-}

-- | The domain name corresponding to the streaming distribution. For example:
-- s5c39gqb8ow64r.cloudfront.net.
sdDomainName :: Lens' StreamingDistribution (Text)
sdDomainName = lens _sdDomainName (\s a -> s { _sdDomainName = a })
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
sdActiveTrustedSigners = lens _sdActiveTrustedSigners (\s a -> s { _sdActiveTrustedSigners = a })
{-# INLINE sdActiveTrustedSigners #-}

-- | The current configuration information for the streaming distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution (StreamingDistributionConfig)
sdStreamingDistributionConfig = lens _sdStreamingDistributionConfig (\s a -> s { _sdStreamingDistributionConfig = a })
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
sdcCallerReference = lens _sdcCallerReference (\s a -> s { _sdcCallerReference = a })
{-# INLINE sdcCallerReference #-}

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig (S3Origin)
sdcS3Origin = lens _sdcS3Origin (\s a -> s { _sdcS3Origin = a })
{-# INLINE sdcS3Origin #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Aliases)
sdcAliases = lens _sdcAliases (\s a -> s { _sdcAliases = a })
{-# INLINE sdcAliases #-}

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig (Text)
sdcComment = lens _sdcComment (\s a -> s { _sdcComment = a })
{-# INLINE sdcComment #-}

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\s a -> s { _sdcLogging = a })
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
sdcTrustedSigners = lens _sdcTrustedSigners (\s a -> s { _sdcTrustedSigners = a })
{-# INLINE sdcTrustedSigners #-}

-- | A complex type that contains information about price class for this
-- streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (PriceClass)
sdcPriceClass = lens _sdcPriceClass (\s a -> s { _sdcPriceClass = a })
{-# INLINE sdcPriceClass #-}

-- | Whether the streaming distribution is enabled to accept end user requests
-- for content.
sdcEnabled :: Lens' StreamingDistributionConfig (Bool)
sdcEnabled = lens _sdcEnabled (\s a -> s { _sdcEnabled = a })
{-# INLINE sdcEnabled #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StreamingDistributionConfig' data type to populate a request.
mkStreamingDistributionConfig :: Text -- ^ 'sdcCallerReference'
                              -> S3Origin -- ^ 'sdcS3Origin'
                              -> Aliases -- ^ 'sdcAliases'
                              -> Text -- ^ 'sdcComment'
                              -> StreamingLoggingConfig -- ^ 'sdcLogging'
                              -> TrustedSigners -- ^ 'sdcTrustedSigners'
                              -> PriceClass -- ^ 'sdcPriceClass'
                              -> Bool -- ^ 'sdcEnabled'
                              -> StreamingDistributionConfig
mkStreamingDistributionConfig p1 p2 p3 p4 p5 p6 p7 p8 = StreamingDistributionConfig
    { _sdcCallerReference = p1
    , _sdcS3Origin = p2
    , _sdcAliases = p3
    , _sdcComment = p4
    , _sdcLogging = p5
    , _sdcTrustedSigners = p6
    , _sdcPriceClass = p7
    , _sdcEnabled = p8
    }
{-# INLINE mkStreamingDistributionConfig #-}

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
sdlMarker = lens _sdlMarker (\s a -> s { _sdlMarker = a })
{-# INLINE sdlMarker #-}

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your streaming
-- distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\s a -> s { _sdlNextMarker = a })
{-# INLINE sdlNextMarker #-}

-- | The value you provided for the MaxItems request parameter.
sdlMaxItems :: Lens' StreamingDistributionList (Integer)
sdlMaxItems = lens _sdlMaxItems (\s a -> s { _sdlMaxItems = a })
{-# INLINE sdlMaxItems #-}

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more distributions
-- in the list.
sdlIsTruncated :: Lens' StreamingDistributionList (Bool)
sdlIsTruncated = lens _sdlIsTruncated (\s a -> s { _sdlIsTruncated = a })
{-# INLINE sdlIsTruncated #-}

-- | The number of streaming distributions that were created by the current AWS
-- account.
sdlQuantity :: Lens' StreamingDistributionList (Integer)
sdlQuantity = lens _sdlQuantity (\s a -> s { _sdlQuantity = a })
{-# INLINE sdlQuantity #-}

-- | A complex type that contains one StreamingDistributionSummary element for
-- each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList ([StreamingDistributionSummary])
sdlItems = lens _sdlItems (\s a -> s { _sdlItems = a })
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
sdsId = lens _sdsId (\s a -> s { _sdsId = a })
{-# INLINE sdsId #-}

-- | Indicates the current status of the distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary (Text)
sdsStatus = lens _sdsStatus (\s a -> s { _sdsStatus = a })
{-# INLINE sdsStatus #-}

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary (ISO8601)
sdsLastModifiedTime = lens _sdsLastModifiedTime (\s a -> s { _sdsLastModifiedTime = a })
{-# INLINE sdsLastModifiedTime #-}

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
sdsDomainName :: Lens' StreamingDistributionSummary (Text)
sdsDomainName = lens _sdsDomainName (\s a -> s { _sdsDomainName = a })
{-# INLINE sdsDomainName #-}

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary (S3Origin)
sdsS3Origin = lens _sdsS3Origin (\s a -> s { _sdsS3Origin = a })
{-# INLINE sdsS3Origin #-}

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary (Aliases)
sdsAliases = lens _sdsAliases (\s a -> s { _sdsAliases = a })
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
sdsTrustedSigners = lens _sdsTrustedSigners (\s a -> s { _sdsTrustedSigners = a })
{-# INLINE sdsTrustedSigners #-}

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary (Text)
sdsComment = lens _sdsComment (\s a -> s { _sdsComment = a })
{-# INLINE sdsComment #-}

sdsPriceClass :: Lens' StreamingDistributionSummary (PriceClass)
sdsPriceClass = lens _sdsPriceClass (\s a -> s { _sdsPriceClass = a })
{-# INLINE sdsPriceClass #-}

-- | Whether the distribution is enabled to accept end user requests for
-- content.
sdsEnabled :: Lens' StreamingDistributionSummary (Bool)
sdsEnabled = lens _sdsEnabled (\s a -> s { _sdsEnabled = a })
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
slcEnabled = lens _slcEnabled (\s a -> s { _slcEnabled = a })
{-# INLINE slcEnabled #-}

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
slcBucket :: Lens' StreamingLoggingConfig (Text)
slcBucket = lens _slcBucket (\s a -> s { _slcBucket = a })
{-# INLINE slcBucket #-}

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, myprefix/. If you
-- want to enable logging, but you do not want to specify a prefix, you still
-- must include an empty Prefix element in the Logging element.
slcPrefix :: Lens' StreamingLoggingConfig (Text)
slcPrefix = lens _slcPrefix (\s a -> s { _slcPrefix = a })
{-# INLINE slcPrefix #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StreamingLoggingConfig' data type to populate a request.
mkStreamingLoggingConfig :: Bool -- ^ 'slcEnabled'
                         -> Text -- ^ 'slcBucket'
                         -> Text -- ^ 'slcPrefix'
                         -> StreamingLoggingConfig
mkStreamingLoggingConfig p1 p2 p3 = StreamingLoggingConfig
    { _slcEnabled = p1
    , _slcBucket = p2
    , _slcPrefix = p3
    }
{-# INLINE mkStreamingLoggingConfig #-}

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
tsEnabled = lens _tsEnabled (\s a -> s { _tsEnabled = a })
{-# INLINE tsEnabled #-}

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners (Integer)
tsQuantity = lens _tsQuantity (\s a -> s { _tsQuantity = a })
{-# INLINE tsQuantity #-}

-- | Optional: A complex type that contains trusted signers for this cache
-- behavior. If Quantity is 0, you can omit Items.
tsItems :: Lens' TrustedSigners ([Text])
tsItems = lens _tsItems (\s a -> s { _tsItems = a })
{-# INLINE tsItems #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TrustedSigners' data type to populate a request.
mkTrustedSigners :: Bool -- ^ 'tsEnabled'
                 -> Integer -- ^ 'tsQuantity'
                 -> TrustedSigners
mkTrustedSigners p1 p2 = TrustedSigners
    { _tsEnabled = p1
    , _tsQuantity = p2
    , _tsItems = mempty
    }
{-# INLINE mkTrustedSigners #-}

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
vcIAMCertificateId = lens _vcIAMCertificateId (\s a -> s { _vcIAMCertificateId = a })
{-# INLINE vcIAMCertificateId #-}

-- | If you want viewers to use HTTPS to request your objects and you're using
-- the CloudFront domain name of your distribution in your object URLs (for
-- example, https://d111111abcdef8.cloudfront.net/logo.jpg), set to true. Omit
-- this value if you are setting an IAMCertificateId.
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate = lens _vcCloudFrontDefaultCertificate (\s a -> s { _vcCloudFrontDefaultCertificate = a })
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
vcSSLSupportMethod = lens _vcSSLSupportMethod (\s a -> s { _vcSSLSupportMethod = a })
{-# INLINE vcSSLSupportMethod #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ViewerCertificate' data type to populate a request.
mkViewerCertificate :: ViewerCertificate
mkViewerCertificate = ViewerCertificate
    { _vcIAMCertificateId = Nothing
    , _vcCloudFrontDefaultCertificate = Nothing
    , _vcSSLSupportMethod = Nothing
    }
{-# INLINE mkViewerCertificate #-}

instance FromXML ViewerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ViewerCertificate"

instance ToXML ViewerCertificate where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ViewerCertificate"
