{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

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
module Network.AWS.CloudFront.V2014_05_31.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

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

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudfront"
        , _svcVersion  = "2014-05-31"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudFront)
deriving instance Generic (Er CloudFront)

instance AWSError (Er CloudFront) where
    awsError = const "CloudFrontError"

instance ServiceError (Er CloudFront) where
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

-- | Use this element to specify whether you want CloudFront to forward cookies
-- to the origin that is associated with this cache behavior. You can specify
-- all, none or whitelist. If you choose All, CloudFront forwards all cookies
-- regardless of how many your application uses.
data ItemSelection
    = ItemSelectionAll -- ^ all
    | ItemSelectionNone -- ^ none
    | ItemSelectionWhitelist -- ^ whitelist
      deriving (Eq, Show, Generic)

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

data Method
    = MethodDelete -- ^ DELETE
    | MethodGet -- ^ GET
    | MethodHead -- ^ HEAD
    | MethodOptions -- ^ OPTIONS
    | MethodPatch -- ^ PATCH
    | MethodPost -- ^ POST
    | MethodPut -- ^ PUT
      deriving (Eq, Show, Generic)

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

-- | The origin protocol policy to apply to your origin.
data OriginProtocolPolicy
    = OriginProtocolPolicyHttpOnly -- ^ http-only
    | OriginProtocolPolicyMatchViewer -- ^ match-viewer
      deriving (Eq, Show, Generic)

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

-- | A complex type that contains information about price class for this
-- streaming distribution.
data PriceClass
    = PriceClassPriceclass100 -- ^ PriceClass_100
    | PriceClassPriceclass200 -- ^ PriceClass_200
    | PriceClassPriceclassAll -- ^ PriceClass_All
      deriving (Eq, Show, Generic)

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
    } deriving (Generic)

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
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML ActiveTrustedSigners where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ActiveTrustedSigners"

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
data Aliases = Aliases
    { _aQuantity :: Integer
      -- ^ The number of CNAMEs, if any, for this distribution.
    , _aItems :: [Text]
      -- ^ Optional: A complex type that contains CNAME elements, if any,
      -- for this distribution. If Quantity is 0, you can omit Items.
    } deriving (Generic)

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
    { _aoQuantity :: Integer
      -- ^ The number of HTTP methods that you want CloudFront to forward to
      -- your origin. Valid values are 2 (for GET and HEAD requests) and 7
      -- (for DELETE, GET, HEAD, OPTIONS, PATCH, POST, and PUT requests).
    , _aoItems :: [Method]
      -- ^ A complex type that contains the HTTP methods that you want
      -- CloudFront to process and forward to your origin.
    } deriving (Generic)

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
    { _ccAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront
      -- processes and forwards to your Amazon S3 bucket or your custom
      -- origin. There are two options: - CloudFront forwards only GET and
      -- HEAD requests. - CloudFront forwards DELETE, GET, HEAD, OPTIONS,
      -- PATCH, POST, and PUT requests. If you choose the second option,
      -- you may need to restrict access to your Amazon S3 bucket or to
      -- your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have
      -- permission to delete objects from your origin.
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
    , _ccTargetOriginId :: Text
      -- ^ The value of ID for the origin that you want CloudFront to route
      -- requests to when a request matches the path pattern either for a
      -- cache behavior or for the default cache behavior.
    , _ccMinTTL :: Integer
      -- ^ The minimum amount of time that you want objects to stay in
      -- CloudFront caches before CloudFront queries your origin to see
      -- whether the object has been updated.You can specify a value from
      -- 0 to 3,153,600,000 seconds (100 years).
    , _ccSmoothStreaming :: Maybe Bool
      -- ^ Indicates whether you want to distribute media files in Microsoft
      -- Smooth Streaming format using the origin that is associated with
      -- this cache behavior. If so, specify true; if not, specify false.
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
    , _ccPathPattern :: Text
      -- ^ The pattern (for example, images/*.jpg) that specifies which
      -- requests you want this cache behavior to apply to. When
      -- CloudFront receives an end-user request, the requested path is
      -- compared with path patterns in the order in which cache behaviors
      -- are listed in the distribution. The path pattern for the default
      -- cache behavior is * and cannot be changed. If the request for an
      -- object does not match the path pattern for any cache behaviors,
      -- CloudFront applies the behavior in the default cache behavior.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML CacheBehaviors where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheBehaviors"

instance ToXML CacheBehaviors where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CacheBehaviors"

-- | The origin access identity's information.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity
    { _cfoaiCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
      -- ^ The current configuration information for the identity.
    , _cfoaiId :: Text
      -- ^ The ID for the origin access identity. For example:
      -- E74FTE3AJFJ256A.
    , _cfoaiS3CanonicalUserId :: Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity,
      -- which you use when giving the origin access identity read
      -- permission to an object in Amazon S3.
    } deriving (Generic)

instance FromXML CloudFrontOriginAccessIdentity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentity"

-- | The identity's configuration information.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig
    { _cfoaicComment :: Text
      -- ^ Any comments you want to include about the origin access
      -- identity.
    , _cfoaicCallerReference :: Text
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
    } deriving (Generic)

instance FromXML CloudFrontOriginAccessIdentityConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentityConfig"

instance ToXML CloudFrontOriginAccessIdentityConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CloudFrontOriginAccessIdentityConfig"

-- | The CloudFrontOriginAccessIdentityList type.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList
    { _cfoailQuantity :: Integer
      -- ^ The number of CloudFront origin access identities that were
      -- created by the current AWS account.
    , _cfoailItems :: [CloudFrontOriginAccessIdentitySummary]
      -- ^ A complex type that contains one
      -- CloudFrontOriginAccessIdentitySummary element for each origin
      -- access identity that was created by the current AWS account.
    , _cfoailMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _cfoailMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _cfoailNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your origin access identities where they left off.
    , _cfoailIsTruncated :: Bool
      -- ^ A flag that indicates whether more origin access identities
      -- remain to be listed. If your results were truncated, you can make
      -- a follow-up pagination request using the Marker request parameter
      -- to retrieve more items in the list.
    } deriving (Generic)

instance FromXML CloudFrontOriginAccessIdentityList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CloudFrontOriginAccessIdentityList"

-- | Summary of the information about a CloudFront origin access identity.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary
    { _cfoaisId :: Text
      -- ^ The ID for the origin access identity. For example:
      -- E74FTE3AJFJ256A.
    , _cfoaisComment :: Text
      -- ^ The comment for this origin access identity, as originally
      -- specified when created.
    , _cfoaisS3CanonicalUserId :: Text
      -- ^ The Amazon S3 canonical user ID for the origin access identity,
      -- which you use when giving the origin access identity read
      -- permission to an object in Amazon S3.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML CookieNames where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CookieNames"

instance ToXML CookieNames where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CookieNames"

-- | A complex type that specifies how CloudFront handles cookies.
data CookiePreference = CookiePreference
    { _cpWhitelistedNames :: Maybe CookieNames
      -- ^ A complex type that specifies the whitelisted cookies, if any,
      -- that you want CloudFront to forward to your origin that is
      -- associated with this cache behavior.
    , _cpForward :: ItemSelection
      -- ^ Use this element to specify whether you want CloudFront to
      -- forward cookies to the origin that is associated with this cache
      -- behavior. You can specify all, none or whitelist. If you choose
      -- All, CloudFront forwards all cookies regardless of how many your
      -- application uses.
    } deriving (Generic)

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
    { _cesResponsePagePath :: Maybe Text
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
    , _cesErrorCode :: Integer
      -- ^ The 4xx or 5xx HTTP status code that you want to customize. For a
      -- list of HTTP status codes that you can customize, see CloudFront
      -- documentation.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML CustomErrorResponses where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CustomErrorResponses"

instance ToXML CustomErrorResponses where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CustomErrorResponses"

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
data CustomOriginConfig = CustomOriginConfig
    { _cocOriginProtocolPolicy :: OriginProtocolPolicy
      -- ^ The origin protocol policy to apply to your origin.
    , _cocHTTPPort :: Integer
      -- ^ The HTTP port the custom origin listens on.
    , _cocHTTPSPort :: Integer
      -- ^ The HTTPS port the custom origin listens on.
    } deriving (Generic)

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
    { _dcbAllowedMethods :: Maybe AllowedMethods
      -- ^ A complex type that controls which HTTP methods CloudFront
      -- processes and forwards to your Amazon S3 bucket or your custom
      -- origin. There are two options: - CloudFront forwards only GET and
      -- HEAD requests. - CloudFront forwards DELETE, GET, HEAD, OPTIONS,
      -- PATCH, POST, and PUT requests. If you choose the second option,
      -- you may need to restrict access to your Amazon S3 bucket or to
      -- your custom origin so users can't perform operations that you
      -- don't want them to. For example, you may not want users to have
      -- permission to delete objects from your origin.
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
    , _dcbTargetOriginId :: Text
      -- ^ The value of ID for the origin that you want CloudFront to route
      -- requests to when a request matches the path pattern either for a
      -- cache behavior or for the default cache behavior.
    , _dcbMinTTL :: Integer
      -- ^ The minimum amount of time that you want objects to stay in
      -- CloudFront caches before CloudFront queries your origin to see
      -- whether the object has been updated.You can specify a value from
      -- 0 to 3,153,600,000 seconds (100 years).
    , _dcbSmoothStreaming :: Maybe Bool
      -- ^ Indicates whether you want to distribute media files in Microsoft
      -- Smooth Streaming format using the origin that is associated with
      -- this cache behavior. If so, specify true; if not, specify false.
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
    } deriving (Generic)

instance FromXML DefaultCacheBehavior where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DefaultCacheBehavior"

instance ToXML DefaultCacheBehavior where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DefaultCacheBehavior"

-- | The distribution's information.
data Distribution = Distribution
    { _dnInProgressInvalidationBatches :: Integer
      -- ^ The number of invalidation batches currently in progress.
    , _dnStatus :: Text
      -- ^ This response element indicates the current status of the
      -- distribution. When the status is Deployed, the distribution's
      -- information is fully propagated throughout the Amazon CloudFront
      -- system.
    , _dnDistributionConfig :: DistributionConfig
      -- ^ The current configuration information for the distribution.
    , _dnLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _dnDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , _dnId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
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
    } deriving (Generic)

instance FromXML Distribution where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Distribution"

-- | The distribution's configuration information.
data DistributionConfig = DistributionConfig
    { _dcEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
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
    , _dcPriceClass :: PriceClass
      -- ^ A complex type that contains information about price class for
      -- this distribution.
    , _dcCustomErrorResponses :: Maybe CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponse
      -- elements.
    , _dcViewerCertificate :: Maybe ViewerCertificate
      -- ^ A complex type that contains information about viewer
      -- certificates for this distribution.
    , _dcRestrictions :: Maybe Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    , _dcOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , _dcLogging :: LoggingConfig
      -- ^ A complex type that controls whether access logs are written for
      -- the distribution.
    , _dcCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , _dcDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you
      -- do not specify a CacheBehavior element or if files don't match
      -- any of the values of PathPattern in CacheBehavior elements.You
      -- must create exactly one default cache behavior.
    , _dcComment :: Text
      -- ^ Any comments you want to include about the distribution.
    , _dcCallerReference :: Text
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
    } deriving (Generic)

instance FromXML DistributionConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionConfig"

instance ToXML DistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DistributionConfig"

-- | The DistributionList type.
data DistributionList = DistributionList
    { _dlQuantity :: Integer
      -- ^ The number of distributions that were created by the current AWS
      -- account.
    , _dlItems :: [DistributionSummary]
      -- ^ A complex type that contains one DistributionSummary element for
      -- each distribution that was created by the current AWS account.
    , _dlMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _dlMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _dlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your distributions where they left off.
    , _dlIsTruncated :: Bool
      -- ^ A flag that indicates whether more distributions remain to be
      -- listed. If your results were truncated, you can make a follow-up
      -- pagination request using the Marker request parameter to retrieve
      -- more distributions in the list.
    } deriving (Generic)

instance FromXML DistributionList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionList"

-- | A summary of the information for an Amazon CloudFront distribution.
data DistributionSummary = DistributionSummary
    { _dsStatus :: Text
      -- ^ This response element indicates the current status of the
      -- distribution. When the status is Deployed, the distribution's
      -- information is fully propagated throughout the Amazon CloudFront
      -- system.
    , _dsEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
    , _dsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this distribution.
    , _dsPriceClass :: PriceClass
    , _dsCustomErrorResponses :: CustomErrorResponses
      -- ^ A complex type that contains zero or more CustomErrorResponses
      -- elements.
    , _dsLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _dsViewerCertificate :: ViewerCertificate
      -- ^ A complex type that contains information about viewer
      -- certificates for this distribution.
    , _dsDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
    , _dsRestrictions :: Restrictions
      -- ^ A complex type that identifies ways in which you want to restrict
      -- distribution of your content.
    , _dsOrigins :: Origins
      -- ^ A complex type that contains information about origins for this
      -- distribution.
    , _dsId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
    , _dsCacheBehaviors :: CacheBehaviors
      -- ^ A complex type that contains zero or more CacheBehavior elements.
    , _dsDefaultCacheBehavior :: DefaultCacheBehavior
      -- ^ A complex type that describes the default cache behavior if you
      -- do not specify a CacheBehavior element or if files don't match
      -- any of the values of PathPattern in CacheBehavior elements.You
      -- must create exactly one default cache behavior.
    , _dsComment :: Text
      -- ^ The comment originally specified when this distribution was
      -- created.
    } deriving (Generic)

instance FromXML DistributionSummary where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DistributionSummary"

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
data ForwardedValues = ForwardedValues
    { _fvHeaders :: Maybe Headers
      -- ^ A complex type that specifies the Headers, if any, that you want
      -- CloudFront to vary upon for this cache behavior.
    , _fvCookies :: CookiePreference
      -- ^ A complex type that specifies how CloudFront handles cookies.
    , _fvQueryString :: Bool
      -- ^ Indicates whether you want CloudFront to forward query strings to
      -- the origin that is associated with this cache behavior. If so,
      -- specify true; if not, specify false.
    } deriving (Generic)

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
    { _grQuantity :: Integer
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
    , _grRestrictionType :: GeoRestrictionType
      -- ^ The method that you want to use to restrict distribution of your
      -- content by country: - none: No geo restriction is enabled,
      -- meaning access to content is not restricted by client geo
      -- location. - blacklist: The Location elements specify the
      -- countries in which you do not want CloudFront to distribute your
      -- content. - whitelist: The Location elements specify the countries
      -- in which you want CloudFront to distribute your content.
    } deriving (Generic)

instance FromXML GeoRestriction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GeoRestriction"

instance ToXML GeoRestriction where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GeoRestriction"

-- | A complex type that specifies the Headers, if any, that you want CloudFront
-- to vary upon for this cache behavior.
data Headers = Headers
    { _hsQuantity :: Integer
      -- ^ The number of different headers that you want CloudFront to
      -- forward to the origin and to vary on for this cache behavior. The
      -- maximum number of headers that you can specify by name is 10. If
      -- you want CloudFront to forward all headers to the origin and vary
      -- on all of them, specify 1 for Quantity and * for Name. If you
      -- don't want CloudFront to forward any additional headers to the
      -- origin or to vary on any headers, specify 0 for Quantity and omit
      -- Items.
    , _hsItems :: [Text]
      -- ^ Optional: A complex type that contains a Name element for each
      -- header that you want CloudFront to forward to the origin and to
      -- vary on for this cache behavior. If Quantity is 0, omit Items.
    } deriving (Generic)

instance FromXML Headers where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Headers"

instance ToXML Headers where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Headers"

-- | The invalidation's information.
data Invalidation = Invalidation
    { _inStatus :: Text
      -- ^ The status of the invalidation request. When the invalidation
      -- batch is finished, the status is Completed.
    , _inInvalidationBatch :: InvalidationBatch
      -- ^ The current invalidation information for the batch request.
    , _inId :: Text
      -- ^ The identifier for the invalidation request. For example:
      -- IDFDVBD632BHDS5.
    , _inCreateTime :: ISO8601
      -- ^ The date and time the invalidation request was first made.
    } deriving (Generic)

instance FromXML Invalidation where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Invalidation"

-- | The current invalidation information for the batch request.
data InvalidationBatch = InvalidationBatch
    { _ibCallerReference :: Text
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
    , _ibPaths :: Paths
      -- ^ The path of the object to invalidate. The path is relative to the
      -- distribution and must begin with a slash (/). You must enclose
      -- each invalidation object with the Path element tags. If the path
      -- includes non-ASCII characters or unsafe characters as defined in
      -- RFC 1783 (http://www.ietf.org/rfc/rfc1738.txt), URL encode those
      -- characters. Do not URL encode any other characters in the path,
      -- or CloudFront will not invalidate the old version of the updated
      -- object.
    } deriving (Generic)

instance FromXML InvalidationBatch where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InvalidationBatch"

instance ToXML InvalidationBatch where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "InvalidationBatch"

-- | Information about invalidation batches.
data InvalidationList = InvalidationList
    { _ilQuantity :: Integer
      -- ^ The number of invalidation batches that were created by the
      -- current AWS account.
    , _ilItems :: [InvalidationSummary]
      -- ^ A complex type that contains one InvalidationSummary element for
      -- each invalidation batch that was created by the current AWS
      -- account.
    , _ilMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _ilMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _ilNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your invalidation batches where they left off.
    , _ilIsTruncated :: Bool
      -- ^ A flag that indicates whether more invalidation batch requests
      -- remain to be listed. If your results were truncated, you can make
      -- a follow-up pagination request using the Marker request parameter
      -- to retrieve more invalidation batches in the list.
    } deriving (Generic)

instance FromXML InvalidationList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InvalidationList"

-- | Summary of an invalidation request.
data InvalidationSummary = InvalidationSummary
    { _iiiiiiiiiiyStatus :: Text
      -- ^ The status of an invalidation request.
    , _iiiiiiiiiiyId :: Text
      -- ^ The unique ID for an invalidation request.
    , _iiiiiiiiiiyCreateTime :: ISO8601
    } deriving (Generic)

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
    } deriving (Generic)

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
    , _lcPrefix :: Text
      -- ^ An optional string that you want CloudFront to prefix to the
      -- access log filenames for this distribution, for example,
      -- myprefix/. If you want to enable logging, but you do not want to
      -- specify a prefix, you still must include an empty Prefix element
      -- in the Logging element.
    , _lcBucket :: Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    , _lcIncludeCookies :: Bool
      -- ^ Specifies whether you want CloudFront to include cookies in
      -- access logs, specify true for IncludeCookies. If you choose to
      -- include cookies in logs, CloudFront logs all cookies regardless
      -- of how you configure the cache behaviors for this distribution.
      -- If you do not want to include cookies when you create a
      -- distribution or if you want to disable include cookies for an
      -- existing distribution, specify false for IncludeCookies.
    } deriving (Generic)

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
    { _onCustomOriginConfig :: Maybe CustomOriginConfig
      -- ^ A complex type that contains information about a custom origin.
      -- If the origin is an Amazon S3 bucket, use the S3OriginConfig
      -- element instead.
    , _onS3OriginConfig :: Maybe S3OriginConfig
      -- ^ A complex type that contains information about the Amazon S3
      -- origin. If the origin is a custom origin, use the
      -- CustomOriginConfig element instead.
    , _onDomainName :: Text
      -- ^ Amazon S3 origins: The DNS name of the Amazon S3 bucket from
      -- which you want CloudFront to get objects for this origin, for
      -- example, myawsbucket.s3.amazonaws.com. Custom origins: The DNS
      -- domain name for the HTTP server from which you want CloudFront to
      -- get objects for this origin, for example, www.example.com.
    , _onId :: Text
      -- ^ A unique identifier for the origin. The value of Id must be
      -- unique within the distribution. You use the value of Id when you
      -- create a cache behavior. The Id identifies the origin that
      -- CloudFront routes a request to when the request matches the path
      -- pattern for that cache behavior.
    } deriving (Generic)

instance FromXML Origin where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Origin"

instance ToXML Origin where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Origin"

-- | A complex type that contains information about origins for this
-- distribution.
data Origins = Origins
    { _osQuantity :: Integer
      -- ^ The number of origins for this distribution.
    , _osItems :: Maybe [Origin]
      -- ^ A complex type that contains origins for this distribution.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML Paths where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Paths"

instance ToXML Paths where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "Paths"

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
data S3Origin = S3Origin
    { _soDomainName :: Text
      -- ^ The DNS name of the S3 origin.
    , _soOriginAccessIdentity :: Text
      -- ^ Your S3 origin's origin access identity.
    } deriving (Generic)

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
    { _szAwsAccountNumber :: Maybe Text
      -- ^ Specifies an AWS account that can create signed URLs. Values:
      -- self, which indicates that the AWS account that was used to
      -- create the distribution can created signed URLs, or an AWS
      -- account number. Omit the dashes in the account number.
    , _szKeyPairIds :: Maybe KeyPairIds
      -- ^ A complex type that lists the active CloudFront key pairs, if
      -- any, that are associated with AwsAccountNumber.
    } deriving (Generic)

instance FromXML Signer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Signer"

-- | The streaming distribution's information.
data StreamingDistribution = StreamingDistribution
    { _sdStatus :: Text
      -- ^ The current status of the streaming distribution. When the status
      -- is Deployed, the distribution's information is fully propagated
      -- throughout the Amazon CloudFront system.
    , _sdStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The current configuration information for the streaming
      -- distribution.
    , _sdLastModifiedTime :: Maybe ISO8601
      -- ^ The date and time the distribution was last modified.
    , _sdDomainName :: Text
      -- ^ The domain name corresponding to the streaming distribution. For
      -- example: s5c39gqb8ow64r.cloudfront.net.
    , _sdId :: Text
      -- ^ The identifier for the streaming distribution. For example:
      -- EGTXBD79H29TRA8.
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
    } deriving (Generic)

instance FromXML StreamingDistribution where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistribution"

-- | The streaming distribution's configuration information.
data StreamingDistributionConfig = StreamingDistributionConfig
    { _sdcEnabled :: Bool
      -- ^ Whether the streaming distribution is enabled to accept end user
      -- requests for content.
    , _sdcAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this streaming distribution.
    , _sdcPriceClass :: PriceClass
      -- ^ A complex type that contains information about price class for
      -- this streaming distribution.
    , _sdcS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3
      -- bucket from which you want CloudFront to get your media files for
      -- distribution.
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
    , _sdcLogging :: StreamingLoggingConfig
      -- ^ A complex type that controls whether access logs are written for
      -- the streaming distribution.
    , _sdcComment :: Text
      -- ^ Any comments you want to include about the streaming
      -- distribution.
    , _sdcCallerReference :: Text
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
    } deriving (Generic)

instance FromXML StreamingDistributionConfig where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistributionConfig"

instance ToXML StreamingDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "StreamingDistributionConfig"

-- | The StreamingDistributionList type.
data StreamingDistributionList = StreamingDistributionList
    { _sdlQuantity :: Integer
      -- ^ The number of streaming distributions that were created by the
      -- current AWS account.
    , _sdlItems :: [StreamingDistributionSummary]
      -- ^ A complex type that contains one StreamingDistributionSummary
      -- element for each distribution that was created by the current AWS
      -- account.
    , _sdlMarker :: Text
      -- ^ The value you provided for the Marker request parameter.
    , _sdlMaxItems :: Integer
      -- ^ The value you provided for the MaxItems request parameter.
    , _sdlNextMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the
      -- value you can use for the Marker request parameter to continue
      -- listing your streaming distributions where they left off.
    , _sdlIsTruncated :: Bool
      -- ^ A flag that indicates whether more streaming distributions remain
      -- to be listed. If your results were truncated, you can make a
      -- follow-up pagination request using the Marker request parameter
      -- to retrieve more distributions in the list.
    } deriving (Generic)

instance FromXML StreamingDistributionList where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "StreamingDistributionList"

-- | A summary of the information for an Amazon CloudFront streaming
-- distribution.
data StreamingDistributionSummary = StreamingDistributionSummary
    { _sdsStatus :: Text
      -- ^ Indicates the current status of the distribution. When the status
      -- is Deployed, the distribution's information is fully propagated
      -- throughout the Amazon CloudFront system.
    , _sdsEnabled :: Bool
      -- ^ Whether the distribution is enabled to accept end user requests
      -- for content.
    , _sdsAliases :: Aliases
      -- ^ A complex type that contains information about CNAMEs (alternate
      -- domain names), if any, for this streaming distribution.
    , _sdsPriceClass :: PriceClass
    , _sdsLastModifiedTime :: ISO8601
      -- ^ The date and time the distribution was last modified.
    , _sdsS3Origin :: S3Origin
      -- ^ A complex type that contains information about the Amazon S3
      -- bucket from which you want CloudFront to get your media files for
      -- distribution.
    , _sdsDomainName :: Text
      -- ^ The domain name corresponding to the distribution. For example:
      -- d604721fxaaqy9.cloudfront.net.
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
    , _sdsId :: Text
      -- ^ The identifier for the distribution. For example:
      -- EDFDVBD632BHDS5.
    , _sdsComment :: Text
      -- ^ The comment originally specified when this distribution was
      -- created.
    } deriving (Generic)

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
    , _slcPrefix :: Text
      -- ^ An optional string that you want CloudFront to prefix to the
      -- access log filenames for this streaming distribution, for
      -- example, myprefix/. If you want to enable logging, but you do not
      -- want to specify a prefix, you still must include an empty Prefix
      -- element in the Logging element.
    , _slcBucket :: Text
      -- ^ The Amazon S3 bucket to store the access logs in, for example,
      -- myawslogbucket.s3.amazonaws.com.
    } deriving (Generic)

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
    } deriving (Generic)

instance FromXML TrustedSigners where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TrustedSigners"

instance ToXML TrustedSigners where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "TrustedSigners"

-- | A complex type that contains information about viewer certificates for this
-- distribution.
data ViewerCertificate = ViewerCertificate
    { _vcSSLSupportMethod :: Maybe SSLSupportMethod
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
    , _vcIAMCertificateId :: Maybe Text
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
    } deriving (Generic)

instance FromXML ViewerCertificate where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ViewerCertificate"

instance ToXML ViewerCertificate where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ViewerCertificate"
