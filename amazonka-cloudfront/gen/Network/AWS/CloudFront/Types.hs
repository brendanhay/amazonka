{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudFront.Types
    (
    -- * Service
      CloudFront
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList
    , cloudFrontOriginAccessIdentityList
    , cfoailIsTruncated
    , cfoailItems
    , cfoailMarker
    , cfoailMaxItems
    , cfoailNextMarker
    , cfoailQuantity

    -- * Invalidation
    , Invalidation
    , invalidation
    , iCreateTime
    , iId
    , iInvalidationBatch
    , iStatus

    -- * SSLSupportMethod
    , SSLSupportMethod (..)

    -- * AllowedMethods
    , AllowedMethods
    , allowedMethods
    , amCachedMethods
    , amItems
    , amQuantity

    -- * CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig
    , cloudFrontOriginAccessIdentityConfig
    , cfoaicCallerReference
    , cfoaicComment

    -- * Origin
    , Origin
    , origin
    , oCustomOriginConfig
    , oDomainName
    , oId
    , oOriginPath
    , oS3OriginConfig

    -- * ViewerProtocolPolicy
    , ViewerProtocolPolicy (..)

    -- * StreamingDistributionList
    , StreamingDistributionList
    , streamingDistributionList
    , sdlIsTruncated
    , sdlItems
    , sdlMarker
    , sdlMaxItems
    , sdlNextMarker
    , sdlQuantity

    -- * StreamingDistributionConfig
    , StreamingDistributionConfig
    , streamingDistributionConfig
    , sdcAliases
    , sdcCallerReference
    , sdcComment
    , sdcEnabled
    , sdcLogging
    , sdcPriceClass
    , sdcS3Origin
    , sdcTrustedSigners

    -- * Signer
    , Signer
    , signer
    , sAwsAccountNumber
    , sKeyPairIds

    -- * CookiePreference
    , CookiePreference
    , cookiePreference
    , cpForward
    , cpWhitelistedNames

    -- * OriginProtocolPolicy
    , OriginProtocolPolicy (..)

    -- * Distribution
    , Distribution
    , distribution
    , dActiveTrustedSigners
    , dDistributionConfig
    , dDomainName
    , dId
    , dInProgressInvalidationBatches
    , dLastModifiedTime
    , dStatus

    -- * CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary
    , cloudFrontOriginAccessIdentitySummary
    , cfoaisComment
    , cfoaisId
    , cfoaisS3CanonicalUserId

    -- * StreamingDistributionSummary
    , StreamingDistributionSummary
    , streamingDistributionSummary
    , sdsAliases
    , sdsComment
    , sdsDomainName
    , sdsEnabled
    , sdsId
    , sdsLastModifiedTime
    , sdsPriceClass
    , sdsS3Origin
    , sdsStatus
    , sdsTrustedSigners

    -- * CustomOriginConfig
    , CustomOriginConfig
    , customOriginConfig
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- * Aliases
    , Aliases
    , aliases
    , aItems
    , aQuantity

    -- * InvalidationBatch
    , InvalidationBatch
    , invalidationBatch
    , ibCallerReference
    , ibPaths

    -- * InvalidationSummary
    , InvalidationSummary
    , invalidationSummary
    , isCreateTime
    , isId
    , isStatus

    -- * DistributionConfig
    , DistributionConfig
    , distributionConfig
    , dcAliases
    , dcCacheBehaviors
    , dcCallerReference
    , dcComment
    , dcCustomErrorResponses
    , dcDefaultCacheBehavior
    , dcDefaultRootObject
    , dcEnabled
    , dcLogging
    , dcOrigins
    , dcPriceClass
    , dcRestrictions
    , dcViewerCertificate

    -- * CacheBehavior
    , CacheBehavior
    , cacheBehavior
    , cbAllowedMethods
    , cbForwardedValues
    , cbMinTTL
    , cbPathPattern
    , cbSmoothStreaming
    , cbTargetOriginId
    , cbTrustedSigners
    , cbViewerProtocolPolicy

    -- * DistributionList
    , DistributionList
    , distributionList
    , dlIsTruncated
    , dlItems
    , dlMarker
    , dlMaxItems
    , dlNextMarker
    , dlQuantity

    -- * KeyPairIds
    , KeyPairIds
    , keyPairIds
    , kpiItems
    , kpiQuantity

    -- * PriceClass
    , PriceClass (..)

    -- * CustomErrorResponses
    , CustomErrorResponses
    , customErrorResponses
    , cerItems
    , cerQuantity

    -- * S3OriginConfig
    , S3OriginConfig
    , s3OriginConfig
    , socOriginAccessIdentity

    -- * GeoRestriction
    , GeoRestriction
    , geoRestriction
    , grItems
    , grQuantity
    , grRestrictionType

    -- * S3Origin
    , S3Origin
    , s3Origin
    , soDomainName
    , soOriginAccessIdentity

    -- * Headers
    , Headers
    , headers
    , hItems
    , hQuantity

    -- * CachedMethods
    , CachedMethods
    , cachedMethods
    , cmItems
    , cmQuantity

    -- * ViewerCertificate
    , ViewerCertificate
    , viewerCertificate
    , vcCloudFrontDefaultCertificate
    , vcIAMCertificateId
    , vcMinimumProtocolVersion
    , vcSSLSupportMethod

    -- * Restrictions
    , Restrictions
    , restrictions
    , rGeoRestriction

    -- * Origins
    , Origins
    , origins
    , oItems
    , oQuantity

    -- * Method
    , Method (..)

    -- * MinimumProtocolVersion
    , MinimumProtocolVersion (..)

    -- * ForwardedValues
    , ForwardedValues
    , forwardedValues
    , fvCookies
    , fvHeaders
    , fvQueryString

    -- * TrustedSigners
    , TrustedSigners
    , trustedSigners
    , tsEnabled
    , tsItems
    , tsQuantity

    -- * ItemSelection
    , ItemSelection (..)

    -- * StreamingLoggingConfig
    , StreamingLoggingConfig
    , streamingLoggingConfig
    , slcBucket
    , slcEnabled
    , slcPrefix

    -- * CookieNames
    , CookieNames
    , cookieNames
    , cnItems
    , cnQuantity

    -- * CustomErrorResponse
    , CustomErrorResponse
    , customErrorResponse
    , cerErrorCachingMinTTL
    , cerErrorCode
    , cerResponseCode
    , cerResponsePagePath

    -- * CacheBehaviors
    , CacheBehaviors
    , cacheBehaviors
    , cbItems
    , cbQuantity

    -- * DefaultCacheBehavior
    , DefaultCacheBehavior
    , defaultCacheBehavior
    , dcbAllowedMethods
    , dcbForwardedValues
    , dcbMinTTL
    , dcbSmoothStreaming
    , dcbTargetOriginId
    , dcbTrustedSigners
    , dcbViewerProtocolPolicy

    -- * InvalidationList
    , InvalidationList
    , invalidationList
    , ilIsTruncated
    , ilItems
    , ilMarker
    , ilMaxItems
    , ilNextMarker
    , ilQuantity

    -- * StreamingDistribution
    , StreamingDistribution
    , streamingDistribution
    , sdActiveTrustedSigners
    , sdDomainName
    , sdId
    , sdLastModifiedTime
    , sdStatus
    , sdStreamingDistributionConfig

    -- * Paths
    , Paths
    , paths
    , pItems
    , pQuantity

    -- * CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity
    , cloudFrontOriginAccessIdentity
    , cfoaiCloudFrontOriginAccessIdentityConfig
    , cfoaiId
    , cfoaiS3CanonicalUserId

    -- * ActiveTrustedSigners
    , ActiveTrustedSigners
    , activeTrustedSigners
    , atsEnabled
    , atsItems
    , atsQuantity

    -- * DistributionSummary
    , DistributionSummary
    , distributionSummary
    , dsAliases
    , dsCacheBehaviors
    , dsComment
    , dsCustomErrorResponses
    , dsDefaultCacheBehavior
    , dsDomainName
    , dsEnabled
    , dsId
    , dsLastModifiedTime
    , dsOrigins
    , dsPriceClass
    , dsRestrictions
    , dsStatus
    , dsViewerCertificate

    -- * GeoRestrictionType
    , GeoRestrictionType (..)

    -- * LoggingConfig
    , LoggingConfig
    , loggingConfig
    , lcBucket
    , lcEnabled
    , lcIncludeCookies
    , lcPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-11-06@ of the Amazon CloudFront service.
data CloudFront

instance AWSService CloudFront where
    type Sg CloudFront = V4
    type Er CloudFront = RESTError

    service = service'
      where
        service' :: Service CloudFront
        service' = Service
            { _svcAbbrev       = "CloudFront"
            , _svcPrefix       = "cloudfront"
            , _svcVersion      = "2014-11-06"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry CloudFront
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://cloudfront.amazonaws.com/doc/2014-11-06/"
{-# INLINE ns #-}

data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList
    { _cfoailIsTruncated :: Bool
    , _cfoailItems       :: List "CloudFrontOriginAccessIdentitySummary" CloudFrontOriginAccessIdentitySummary
    , _cfoailMarker      :: Text
    , _cfoailMaxItems    :: Int
    , _cfoailNextMarker  :: Maybe Text
    , _cfoailQuantity    :: Int
    } deriving (Eq, Read, Show)

-- | 'CloudFrontOriginAccessIdentityList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoailIsTruncated' @::@ 'Bool'
--
-- * 'cfoailItems' @::@ ['CloudFrontOriginAccessIdentitySummary']
--
-- * 'cfoailMarker' @::@ 'Text'
--
-- * 'cfoailMaxItems' @::@ 'Int'
--
-- * 'cfoailNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'cfoailQuantity' @::@ 'Int'
--
cloudFrontOriginAccessIdentityList :: Text -- ^ 'cfoailMarker'
                                   -> Int -- ^ 'cfoailMaxItems'
                                   -> Bool -- ^ 'cfoailIsTruncated'
                                   -> Int -- ^ 'cfoailQuantity'
                                   -> CloudFrontOriginAccessIdentityList
cloudFrontOriginAccessIdentityList p1 p2 p3 p4 = CloudFrontOriginAccessIdentityList
    { _cfoailMarker      = p1
    , _cfoailMaxItems    = p2
    , _cfoailIsTruncated = p3
    , _cfoailQuantity    = p4
    , _cfoailNextMarker  = Nothing
    , _cfoailItems       = mempty
    }

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more items in the list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList Bool
cfoailIsTruncated =
    lens _cfoailIsTruncated (\s a -> s { _cfoailIsTruncated = a })

-- | A complex type that contains one CloudFrontOriginAccessIdentitySummary
-- element for each origin access identity that was created by the current AWS
-- account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList [CloudFrontOriginAccessIdentitySummary]
cfoailItems = lens _cfoailItems (\s a -> s { _cfoailItems = a }) . _List

-- | The value you provided for the Marker request parameter.
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList Text
cfoailMarker = lens _cfoailMarker (\s a -> s { _cfoailMarker = a })

-- | The value you provided for the MaxItems request parameter.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailMaxItems = lens _cfoailMaxItems (\s a -> s { _cfoailMaxItems = a })

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your origin
-- access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\s a -> s { _cfoailNextMarker = a })

-- | The number of CloudFront origin access identities that were created by the
-- current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailQuantity = lens _cfoailQuantity (\s a -> s { _cfoailQuantity = a })

instance FromXML CloudFrontOriginAccessIdentityList where
    parseXML x = CloudFrontOriginAccessIdentityList
        <$> x .@  "IsTruncated"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Marker"
        <*> x .@  "MaxItems"
        <*> x .@? "NextMarker"
        <*> x .@  "Quantity"

instance ToXML CloudFrontOriginAccessIdentityList where
    toXML CloudFrontOriginAccessIdentityList{..} = nodes "CloudFrontOriginAccessIdentityList"
        [ "Marker"      =@ _cfoailMarker
        , "NextMarker"  =@ _cfoailNextMarker
        , "MaxItems"    =@ _cfoailMaxItems
        , "IsTruncated" =@ _cfoailIsTruncated
        , "Quantity"    =@ _cfoailQuantity
        , "Items"       =@ _cfoailItems
        ]

data Invalidation = Invalidation
    { _iCreateTime        :: ISO8601
    , _iId                :: Text
    , _iInvalidationBatch :: InvalidationBatch
    , _iStatus            :: Text
    } deriving (Eq, Read, Show)

-- | 'Invalidation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iCreateTime' @::@ 'UTCTime'
--
-- * 'iId' @::@ 'Text'
--
-- * 'iInvalidationBatch' @::@ 'InvalidationBatch'
--
-- * 'iStatus' @::@ 'Text'
--
invalidation :: Text -- ^ 'iId'
             -> Text -- ^ 'iStatus'
             -> UTCTime -- ^ 'iCreateTime'
             -> InvalidationBatch -- ^ 'iInvalidationBatch'
             -> Invalidation
invalidation p1 p2 p3 p4 = Invalidation
    { _iId                = p1
    , _iStatus            = p2
    , _iCreateTime        = withIso _Time (const id) p3
    , _iInvalidationBatch = p4
    }

-- | The date and time the invalidation request was first made.
iCreateTime :: Lens' Invalidation UTCTime
iCreateTime = lens _iCreateTime (\s a -> s { _iCreateTime = a }) . _Time

-- | The identifier for the invalidation request. For example: IDFDVBD632BHDS5.
iId :: Lens' Invalidation Text
iId = lens _iId (\s a -> s { _iId = a })

-- | The current invalidation information for the batch request.
iInvalidationBatch :: Lens' Invalidation InvalidationBatch
iInvalidationBatch =
    lens _iInvalidationBatch (\s a -> s { _iInvalidationBatch = a })

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is Completed.
iStatus :: Lens' Invalidation Text
iStatus = lens _iStatus (\s a -> s { _iStatus = a })

instance FromXML Invalidation where
    parseXML x = Invalidation
        <$> x .@  "CreateTime"
        <*> x .@  "Id"
        <*> x .@  "InvalidationBatch"
        <*> x .@  "Status"

instance ToXMLRoot Invalidation where
    toXMLRoot Invalidation{..} = namespaced ns "Invalidation"
        [ "Id"                =@ _iId
        , "Status"            =@ _iStatus
        , "CreateTime"        =@ _iCreateTime
        , "InvalidationBatch" =@ _iInvalidationBatch
        ]

instance ToXML Invalidation

data SSLSupportMethod
    = SniOnly -- ^ sni-only
    | Vip     -- ^ vip
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable SSLSupportMethod

instance FromText SSLSupportMethod where
    parser = takeLowerText >>= \case
        "sni-only" -> pure SniOnly
        "vip"      -> pure Vip
        e          -> fail $
            "Failure parsing SSLSupportMethod from " ++ show e

instance ToText SSLSupportMethod where
    toText = \case
        SniOnly -> "sni-only"
        Vip     -> "vip"

instance ToByteString SSLSupportMethod
instance ToHeader     SSLSupportMethod
instance ToQuery      SSLSupportMethod

instance FromXML SSLSupportMethod where
    parseXML = parseXMLText "SSLSupportMethod"

instance ToXML SSLSupportMethod where
    toXML = toXMLText

data AllowedMethods = AllowedMethods
    { _amCachedMethods :: Maybe CachedMethods
    , _amItems         :: List "Method" Method
    , _amQuantity      :: Int
    } deriving (Eq, Read, Show)

-- | 'AllowedMethods' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amCachedMethods' @::@ 'Maybe' 'CachedMethods'
--
-- * 'amItems' @::@ ['Method']
--
-- * 'amQuantity' @::@ 'Int'
--
allowedMethods :: Int -- ^ 'amQuantity'
               -> AllowedMethods
allowedMethods p1 = AllowedMethods
    { _amQuantity      = p1
    , _amItems         = mempty
    , _amCachedMethods = Nothing
    }

amCachedMethods :: Lens' AllowedMethods (Maybe CachedMethods)
amCachedMethods = lens _amCachedMethods (\s a -> s { _amCachedMethods = a })

-- | A complex type that contains the HTTP methods that you want CloudFront to
-- process and forward to your origin.
amItems :: Lens' AllowedMethods [Method]
amItems = lens _amItems (\s a -> s { _amItems = a }) . _List

-- | The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for GET and HEAD requests), 3 (for GET, HEAD and
-- OPTIONS requests) and 7 (for GET, HEAD, OPTIONS, PUT, PATCH, POST, and DELETE
-- requests).
amQuantity :: Lens' AllowedMethods Int
amQuantity = lens _amQuantity (\s a -> s { _amQuantity = a })

instance FromXML AllowedMethods where
    parseXML x = AllowedMethods
        <$> x .@? "CachedMethods"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML AllowedMethods where
    toXML AllowedMethods{..} = nodes "AllowedMethods"
        [ "Quantity"      =@ _amQuantity
        , "Items"         =@ _amItems
        , "CachedMethods" =@ _amCachedMethods
        ]

data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig
    { _cfoaicCallerReference :: Text
    , _cfoaicComment         :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CloudFrontOriginAccessIdentityConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaicCallerReference' @::@ 'Text'
--
-- * 'cfoaicComment' @::@ 'Text'
--
cloudFrontOriginAccessIdentityConfig :: Text -- ^ 'cfoaicCallerReference'
                                     -> Text -- ^ 'cfoaicComment'
                                     -> CloudFrontOriginAccessIdentityConfig
cloudFrontOriginAccessIdentityConfig p1 p2 = CloudFrontOriginAccessIdentityConfig
    { _cfoaicCallerReference = p1
    , _cfoaicComment         = p2
    }

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the
-- CloudFrontOriginAccessIdentityConfig object), a new origin access identity is
-- created. If the CallerReference is a value you already sent in a previous
-- request to create an identity, and the content of the
-- CloudFrontOriginAccessIdentityConfig is identical to the original request
-- (ignoring white space), the response includes the same information returned
-- to the original request. If the CallerReference is a value you already sent
-- in a previous request to create an identity but the content of the
-- CloudFrontOriginAccessIdentityConfig is different from the original request,
-- CloudFront returns a CloudFrontOriginAccessIdentityAlreadyExists error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicCallerReference =
    lens _cfoaicCallerReference (\s a -> s { _cfoaicCallerReference = a })

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicComment = lens _cfoaicComment (\s a -> s { _cfoaicComment = a })

instance FromXML CloudFrontOriginAccessIdentityConfig where
    parseXML x = CloudFrontOriginAccessIdentityConfig
        <$> x .@  "CallerReference"
        <*> x .@  "Comment"

instance ToXMLRoot CloudFrontOriginAccessIdentityConfig where
    toXMLRoot CloudFrontOriginAccessIdentityConfig{..} = namespaced ns "CloudFrontOriginAccessIdentityConfig"
        [ "CallerReference" =@ _cfoaicCallerReference
        , "Comment"         =@ _cfoaicComment
        ]

instance ToXML CloudFrontOriginAccessIdentityConfig

data Origin = Origin
    { _oCustomOriginConfig :: Maybe CustomOriginConfig
    , _oDomainName         :: Text
    , _oId                 :: Text
    , _oOriginPath         :: Maybe Text
    , _oS3OriginConfig     :: Maybe S3OriginConfig
    } deriving (Eq, Read, Show)

-- | 'Origin' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oCustomOriginConfig' @::@ 'Maybe' 'CustomOriginConfig'
--
-- * 'oDomainName' @::@ 'Text'
--
-- * 'oId' @::@ 'Text'
--
-- * 'oOriginPath' @::@ 'Maybe' 'Text'
--
-- * 'oS3OriginConfig' @::@ 'Maybe' 'S3OriginConfig'
--
origin :: Text -- ^ 'oId'
       -> Text -- ^ 'oDomainName'
       -> Origin
origin p1 p2 = Origin
    { _oId                 = p1
    , _oDomainName         = p2
    , _oOriginPath         = Nothing
    , _oS3OriginConfig     = Nothing
    , _oCustomOriginConfig = Nothing
    }

-- | A complex type that contains information about a custom origin. If the origin
-- is an Amazon S3 bucket, use the S3OriginConfig element instead.
oCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oCustomOriginConfig =
    lens _oCustomOriginConfig (\s a -> s { _oCustomOriginConfig = a })

-- | Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you want
-- CloudFront to get objects for this origin, for example,
-- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for the
-- HTTP server from which you want CloudFront to get objects for this origin,
-- for example, www.example.com.
oDomainName :: Lens' Origin Text
oDomainName = lens _oDomainName (\s a -> s { _oDomainName = a })

-- | A unique identifier for the origin. The value of Id must be unique within the
-- distribution. You use the value of Id when you create a cache behavior. The
-- Id identifies the origin that CloudFront routes a request to when the request
-- matches the path pattern for that cache behavior.
oId :: Lens' Origin Text
oId = lens _oId (\s a -> s { _oId = a })

-- | An optional element that causes CloudFront to request your content from a
-- directory in your Amazon S3 bucket or your custom origin. When you include
-- the OriginPath element, specify the directory name, beginning with a /.
-- CloudFront appends the directory name to the value of DomainName.
oOriginPath :: Lens' Origin (Maybe Text)
oOriginPath = lens _oOriginPath (\s a -> s { _oOriginPath = a })

-- | A complex type that contains information about the Amazon S3 origin. If the
-- origin is a custom origin, use the CustomOriginConfig element instead.
oS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oS3OriginConfig = lens _oS3OriginConfig (\s a -> s { _oS3OriginConfig = a })

instance FromXML Origin where
    parseXML x = Origin
        <$> x .@? "CustomOriginConfig"
        <*> x .@  "DomainName"
        <*> x .@  "Id"
        <*> x .@? "OriginPath"
        <*> x .@? "S3OriginConfig"

instance ToXML Origin where
    toXML Origin{..} = nodes "Origin"
        [ "Id"                 =@ _oId
        , "DomainName"         =@ _oDomainName
        , "OriginPath"         =@ _oOriginPath
        , "S3OriginConfig"     =@ _oS3OriginConfig
        , "CustomOriginConfig" =@ _oCustomOriginConfig
        ]

data ViewerProtocolPolicy
    = AllowAll        -- ^ allow-all
    | HttpsOnly       -- ^ https-only
    | RedirectToHttps -- ^ redirect-to-https
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ViewerProtocolPolicy

instance FromText ViewerProtocolPolicy where
    parser = takeLowerText >>= \case
        "allow-all"         -> pure AllowAll
        "https-only"        -> pure HttpsOnly
        "redirect-to-https" -> pure RedirectToHttps
        e                   -> fail $
            "Failure parsing ViewerProtocolPolicy from " ++ show e

instance ToText ViewerProtocolPolicy where
    toText = \case
        AllowAll        -> "allow-all"
        HttpsOnly       -> "https-only"
        RedirectToHttps -> "redirect-to-https"

instance ToByteString ViewerProtocolPolicy
instance ToHeader     ViewerProtocolPolicy
instance ToQuery      ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
    parseXML = parseXMLText "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
    toXML = toXMLText

data StreamingDistributionList = StreamingDistributionList
    { _sdlIsTruncated :: Bool
    , _sdlItems       :: List "StreamingDistributionSummary" StreamingDistributionSummary
    , _sdlMarker      :: Text
    , _sdlMaxItems    :: Int
    , _sdlNextMarker  :: Maybe Text
    , _sdlQuantity    :: Int
    } deriving (Eq, Read, Show)

-- | 'StreamingDistributionList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdlIsTruncated' @::@ 'Bool'
--
-- * 'sdlItems' @::@ ['StreamingDistributionSummary']
--
-- * 'sdlMarker' @::@ 'Text'
--
-- * 'sdlMaxItems' @::@ 'Int'
--
-- * 'sdlNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'sdlQuantity' @::@ 'Int'
--
streamingDistributionList :: Text -- ^ 'sdlMarker'
                          -> Int -- ^ 'sdlMaxItems'
                          -> Bool -- ^ 'sdlIsTruncated'
                          -> Int -- ^ 'sdlQuantity'
                          -> StreamingDistributionList
streamingDistributionList p1 p2 p3 p4 = StreamingDistributionList
    { _sdlMarker      = p1
    , _sdlMaxItems    = p2
    , _sdlIsTruncated = p3
    , _sdlQuantity    = p4
    , _sdlNextMarker  = Nothing
    , _sdlItems       = mempty
    }

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more distributions in
-- the list.
sdlIsTruncated :: Lens' StreamingDistributionList Bool
sdlIsTruncated = lens _sdlIsTruncated (\s a -> s { _sdlIsTruncated = a })

-- | A complex type that contains one StreamingDistributionSummary element for
-- each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList [StreamingDistributionSummary]
sdlItems = lens _sdlItems (\s a -> s { _sdlItems = a }) . _List

-- | The value you provided for the Marker request parameter.
sdlMarker :: Lens' StreamingDistributionList Text
sdlMarker = lens _sdlMarker (\s a -> s { _sdlMarker = a })

-- | The value you provided for the MaxItems request parameter.
sdlMaxItems :: Lens' StreamingDistributionList Int
sdlMaxItems = lens _sdlMaxItems (\s a -> s { _sdlMaxItems = a })

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your streaming
-- distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\s a -> s { _sdlNextMarker = a })

-- | The number of streaming distributions that were created by the current AWS
-- account.
sdlQuantity :: Lens' StreamingDistributionList Int
sdlQuantity = lens _sdlQuantity (\s a -> s { _sdlQuantity = a })

instance FromXML StreamingDistributionList where
    parseXML x = StreamingDistributionList
        <$> x .@  "IsTruncated"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Marker"
        <*> x .@  "MaxItems"
        <*> x .@? "NextMarker"
        <*> x .@  "Quantity"

instance ToXML StreamingDistributionList where
    toXML StreamingDistributionList{..} = nodes "StreamingDistributionList"
        [ "Marker"      =@ _sdlMarker
        , "NextMarker"  =@ _sdlNextMarker
        , "MaxItems"    =@ _sdlMaxItems
        , "IsTruncated" =@ _sdlIsTruncated
        , "Quantity"    =@ _sdlQuantity
        , "Items"       =@ _sdlItems
        ]

data StreamingDistributionConfig = StreamingDistributionConfig
    { _sdcAliases         :: Maybe Aliases
    , _sdcCallerReference :: Text
    , _sdcComment         :: Text
    , _sdcEnabled         :: Bool
    , _sdcLogging         :: Maybe StreamingLoggingConfig
    , _sdcPriceClass      :: Maybe PriceClass
    , _sdcS3Origin        :: S3Origin
    , _sdcTrustedSigners  :: TrustedSigners
    } deriving (Eq, Read, Show)

-- | 'StreamingDistributionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcAliases' @::@ 'Maybe' 'Aliases'
--
-- * 'sdcCallerReference' @::@ 'Text'
--
-- * 'sdcComment' @::@ 'Text'
--
-- * 'sdcEnabled' @::@ 'Bool'
--
-- * 'sdcLogging' @::@ 'Maybe' 'StreamingLoggingConfig'
--
-- * 'sdcPriceClass' @::@ 'Maybe' 'PriceClass'
--
-- * 'sdcS3Origin' @::@ 'S3Origin'
--
-- * 'sdcTrustedSigners' @::@ 'TrustedSigners'
--
streamingDistributionConfig :: Text -- ^ 'sdcCallerReference'
                            -> S3Origin -- ^ 'sdcS3Origin'
                            -> Text -- ^ 'sdcComment'
                            -> TrustedSigners -- ^ 'sdcTrustedSigners'
                            -> Bool -- ^ 'sdcEnabled'
                            -> StreamingDistributionConfig
streamingDistributionConfig p1 p2 p3 p4 p5 = StreamingDistributionConfig
    { _sdcCallerReference = p1
    , _sdcS3Origin        = p2
    , _sdcComment         = p3
    , _sdcTrustedSigners  = p4
    , _sdcEnabled         = p5
    , _sdcAliases         = Nothing
    , _sdcLogging         = Nothing
    , _sdcPriceClass      = Nothing
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Maybe Aliases)
sdcAliases = lens _sdcAliases (\s a -> s { _sdcAliases = a })

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the
-- StreamingDistributionConfig object), a new streaming distribution is created.
-- If the CallerReference is a value you already sent in a previous request to
-- create a streaming distribution, and the content of the
-- StreamingDistributionConfig is identical to the original request (ignoring
-- white space), the response includes the same information returned to the
-- original request. If the CallerReference is a value you already sent in a
-- previous request to create a streaming distribution but the content of the
-- StreamingDistributionConfig is different from the original request,
-- CloudFront returns a DistributionAlreadyExists error.
sdcCallerReference :: Lens' StreamingDistributionConfig Text
sdcCallerReference =
    lens _sdcCallerReference (\s a -> s { _sdcCallerReference = a })

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig Text
sdcComment = lens _sdcComment (\s a -> s { _sdcComment = a })

-- | Whether the streaming distribution is enabled to accept end user requests for
-- content.
sdcEnabled :: Lens' StreamingDistributionConfig Bool
sdcEnabled = lens _sdcEnabled (\s a -> s { _sdcEnabled = a })

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (Maybe StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\s a -> s { _sdcLogging = a })

-- | A complex type that contains information about price class for this streaming
-- distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (Maybe PriceClass)
sdcPriceClass = lens _sdcPriceClass (\s a -> s { _sdcPriceClass = a })

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = lens _sdcS3Origin (\s a -> s { _sdcS3Origin = a })

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for Quantity.
-- Omit Items. To add, change, or remove one or more trusted signers, change
-- Enabled to true (if it's currently false), change Quantity as applicable, and
-- specify all of the trusted signers that you want to include in the updated
-- distribution.
sdcTrustedSigners :: Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners =
    lens _sdcTrustedSigners (\s a -> s { _sdcTrustedSigners = a })

instance FromXML StreamingDistributionConfig where
    parseXML x = StreamingDistributionConfig
        <$> x .@? "Aliases"
        <*> x .@  "CallerReference"
        <*> x .@  "Comment"
        <*> x .@  "Enabled"
        <*> x .@? "Logging"
        <*> x .@? "PriceClass"
        <*> x .@  "S3Origin"
        <*> x .@  "TrustedSigners"

instance ToXMLRoot StreamingDistributionConfig where
    toXMLRoot StreamingDistributionConfig{..} = namespaced ns "StreamingDistributionConfig"
        [ "CallerReference" =@ _sdcCallerReference
        , "S3Origin"        =@ _sdcS3Origin
        , "Aliases"         =@ _sdcAliases
        , "Comment"         =@ _sdcComment
        , "Logging"         =@ _sdcLogging
        , "TrustedSigners"  =@ _sdcTrustedSigners
        , "PriceClass"      =@ _sdcPriceClass
        , "Enabled"         =@ _sdcEnabled
        ]

instance ToXML StreamingDistributionConfig

data Signer = Signer
    { _sAwsAccountNumber :: Maybe Text
    , _sKeyPairIds       :: Maybe KeyPairIds
    } deriving (Eq, Read, Show)

-- | 'Signer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sAwsAccountNumber' @::@ 'Maybe' 'Text'
--
-- * 'sKeyPairIds' @::@ 'Maybe' 'KeyPairIds'
--
signer :: Signer
signer = Signer
    { _sAwsAccountNumber = Nothing
    , _sKeyPairIds       = Nothing
    }

-- | Specifies an AWS account that can create signed URLs. Values: self, which
-- indicates that the AWS account that was used to create the distribution can
-- created signed URLs, or an AWS account number. Omit the dashes in the account
-- number.
sAwsAccountNumber :: Lens' Signer (Maybe Text)
sAwsAccountNumber =
    lens _sAwsAccountNumber (\s a -> s { _sAwsAccountNumber = a })

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
sKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sKeyPairIds = lens _sKeyPairIds (\s a -> s { _sKeyPairIds = a })

instance FromXML Signer where
    parseXML x = Signer
        <$> x .@? "AwsAccountNumber"
        <*> x .@? "KeyPairIds"

instance ToXML Signer where
    toXML Signer{..} = nodes "Signer"
        [ "AwsAccountNumber" =@ _sAwsAccountNumber
        , "KeyPairIds"       =@ _sKeyPairIds
        ]

data CookiePreference = CookiePreference
    { _cpForward          :: ItemSelection
    , _cpWhitelistedNames :: Maybe CookieNames
    } deriving (Eq, Read, Show)

-- | 'CookiePreference' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpForward' @::@ 'ItemSelection'
--
-- * 'cpWhitelistedNames' @::@ 'Maybe' 'CookieNames'
--
cookiePreference :: ItemSelection -- ^ 'cpForward'
                 -> CookiePreference
cookiePreference p1 = CookiePreference
    { _cpForward          = p1
    , _cpWhitelistedNames = Nothing
    }

-- | Use this element to specify whether you want CloudFront to forward cookies to
-- the origin that is associated with this cache behavior. You can specify all,
-- none or whitelist. If you choose All, CloudFront forwards all cookies
-- regardless of how many your application uses.
cpForward :: Lens' CookiePreference ItemSelection
cpForward = lens _cpForward (\s a -> s { _cpForward = a })

-- | A complex type that specifies the whitelisted cookies, if any, that you want
-- CloudFront to forward to your origin that is associated with this cache
-- behavior.
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames =
    lens _cpWhitelistedNames (\s a -> s { _cpWhitelistedNames = a })

instance FromXML CookiePreference where
    parseXML x = CookiePreference
        <$> x .@  "Forward"
        <*> x .@? "WhitelistedNames"

instance ToXML CookiePreference where
    toXML CookiePreference{..} = nodes "CookiePreference"
        [ "Forward"          =@ _cpForward
        , "WhitelistedNames" =@ _cpWhitelistedNames
        ]

data OriginProtocolPolicy
    = HttpOnly    -- ^ http-only
    | MatchViewer -- ^ match-viewer
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable OriginProtocolPolicy

instance FromText OriginProtocolPolicy where
    parser = takeLowerText >>= \case
        "http-only"    -> pure HttpOnly
        "match-viewer" -> pure MatchViewer
        e              -> fail $
            "Failure parsing OriginProtocolPolicy from " ++ show e

instance ToText OriginProtocolPolicy where
    toText = \case
        HttpOnly    -> "http-only"
        MatchViewer -> "match-viewer"

instance ToByteString OriginProtocolPolicy
instance ToHeader     OriginProtocolPolicy
instance ToQuery      OriginProtocolPolicy

instance FromXML OriginProtocolPolicy where
    parseXML = parseXMLText "OriginProtocolPolicy"

instance ToXML OriginProtocolPolicy where
    toXML = toXMLText

data Distribution = Distribution
    { _dActiveTrustedSigners          :: ActiveTrustedSigners
    , _dDistributionConfig            :: DistributionConfig
    , _dDomainName                    :: Text
    , _dId                            :: Text
    , _dInProgressInvalidationBatches :: Int
    , _dLastModifiedTime              :: ISO8601
    , _dStatus                        :: Text
    } deriving (Eq, Read, Show)

-- | 'Distribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dActiveTrustedSigners' @::@ 'ActiveTrustedSigners'
--
-- * 'dDistributionConfig' @::@ 'DistributionConfig'
--
-- * 'dDomainName' @::@ 'Text'
--
-- * 'dId' @::@ 'Text'
--
-- * 'dInProgressInvalidationBatches' @::@ 'Int'
--
-- * 'dLastModifiedTime' @::@ 'UTCTime'
--
-- * 'dStatus' @::@ 'Text'
--
distribution :: Text -- ^ 'dId'
             -> Text -- ^ 'dStatus'
             -> UTCTime -- ^ 'dLastModifiedTime'
             -> Int -- ^ 'dInProgressInvalidationBatches'
             -> Text -- ^ 'dDomainName'
             -> ActiveTrustedSigners -- ^ 'dActiveTrustedSigners'
             -> DistributionConfig -- ^ 'dDistributionConfig'
             -> Distribution
distribution p1 p2 p3 p4 p5 p6 p7 = Distribution
    { _dId                            = p1
    , _dStatus                        = p2
    , _dLastModifiedTime              = withIso _Time (const id) p3
    , _dInProgressInvalidationBatches = p4
    , _dDomainName                    = p5
    , _dActiveTrustedSigners          = p6
    , _dDistributionConfig            = p7
    }

-- | CloudFront automatically adds this element to the response only if you've set
-- up the distribution to serve private content with signed URLs. The element
-- lists the key pair IDs that CloudFront is aware of for each trusted signer.
-- The Signer child element lists the AWS account number of the trusted signer
-- (or an empty Self element if the signer is you). The Signer element also
-- includes the IDs of any active key pairs associated with the trusted signer's
-- AWS account. If no KeyPairId element appears for a Signer, that signer can't
-- create working signed URLs.
dActiveTrustedSigners :: Lens' Distribution ActiveTrustedSigners
dActiveTrustedSigners =
    lens _dActiveTrustedSigners (\s a -> s { _dActiveTrustedSigners = a })

-- | The current configuration information for the distribution.
dDistributionConfig :: Lens' Distribution DistributionConfig
dDistributionConfig =
    lens _dDistributionConfig (\s a -> s { _dDistributionConfig = a })

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dDomainName :: Lens' Distribution Text
dDomainName = lens _dDomainName (\s a -> s { _dDomainName = a })

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dId :: Lens' Distribution Text
dId = lens _dId (\s a -> s { _dId = a })

-- | The number of invalidation batches currently in progress.
dInProgressInvalidationBatches :: Lens' Distribution Int
dInProgressInvalidationBatches =
    lens _dInProgressInvalidationBatches
        (\s a -> s { _dInProgressInvalidationBatches = a })

-- | The date and time the distribution was last modified.
dLastModifiedTime :: Lens' Distribution UTCTime
dLastModifiedTime =
    lens _dLastModifiedTime (\s a -> s { _dLastModifiedTime = a })
        . _Time

-- | This response element indicates the current status of the distribution. When
-- the status is Deployed, the distribution's information is fully propagated
-- throughout the Amazon CloudFront system.
dStatus :: Lens' Distribution Text
dStatus = lens _dStatus (\s a -> s { _dStatus = a })

instance FromXML Distribution where
    parseXML x = Distribution
        <$> x .@  "ActiveTrustedSigners"
        <*> x .@  "DistributionConfig"
        <*> x .@  "DomainName"
        <*> x .@  "Id"
        <*> x .@  "InProgressInvalidationBatches"
        <*> x .@  "LastModifiedTime"
        <*> x .@  "Status"

instance ToXMLRoot Distribution where
    toXMLRoot Distribution{..} = namespaced ns "Distribution"
        [ "Id"                            =@ _dId
        , "Status"                        =@ _dStatus
        , "LastModifiedTime"              =@ _dLastModifiedTime
        , "InProgressInvalidationBatches" =@ _dInProgressInvalidationBatches
        , "DomainName"                    =@ _dDomainName
        , "ActiveTrustedSigners"          =@ _dActiveTrustedSigners
        , "DistributionConfig"            =@ _dDistributionConfig
        ]

instance ToXML Distribution

data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary
    { _cfoaisComment           :: Text
    , _cfoaisId                :: Text
    , _cfoaisS3CanonicalUserId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CloudFrontOriginAccessIdentitySummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaisComment' @::@ 'Text'
--
-- * 'cfoaisId' @::@ 'Text'
--
-- * 'cfoaisS3CanonicalUserId' @::@ 'Text'
--
cloudFrontOriginAccessIdentitySummary :: Text -- ^ 'cfoaisId'
                                      -> Text -- ^ 'cfoaisS3CanonicalUserId'
                                      -> Text -- ^ 'cfoaisComment'
                                      -> CloudFrontOriginAccessIdentitySummary
cloudFrontOriginAccessIdentitySummary p1 p2 p3 = CloudFrontOriginAccessIdentitySummary
    { _cfoaisId                = p1
    , _cfoaisS3CanonicalUserId = p2
    , _cfoaisComment           = p3
    }

-- | The comment for this origin access identity, as originally specified when
-- created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisComment = lens _cfoaisComment (\s a -> s { _cfoaisComment = a })

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisId = lens _cfoaisId (\s a -> s { _cfoaisId = a })

-- | The Amazon S3 canonical user ID for the origin access identity, which you use
-- when giving the origin access identity read permission to an object in Amazon
-- S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisS3CanonicalUserId =
    lens _cfoaisS3CanonicalUserId (\s a -> s { _cfoaisS3CanonicalUserId = a })

instance FromXML CloudFrontOriginAccessIdentitySummary where
    parseXML x = CloudFrontOriginAccessIdentitySummary
        <$> x .@  "Comment"
        <*> x .@  "Id"
        <*> x .@  "S3CanonicalUserId"

instance ToXML CloudFrontOriginAccessIdentitySummary where
    toXML CloudFrontOriginAccessIdentitySummary{..} = nodes "CloudFrontOriginAccessIdentitySummary"
        [ "Id"                =@ _cfoaisId
        , "S3CanonicalUserId" =@ _cfoaisS3CanonicalUserId
        , "Comment"           =@ _cfoaisComment
        ]

data StreamingDistributionSummary = StreamingDistributionSummary
    { _sdsAliases          :: Aliases
    , _sdsComment          :: Text
    , _sdsDomainName       :: Text
    , _sdsEnabled          :: Bool
    , _sdsId               :: Text
    , _sdsLastModifiedTime :: ISO8601
    , _sdsPriceClass       :: PriceClass
    , _sdsS3Origin         :: S3Origin
    , _sdsStatus           :: Text
    , _sdsTrustedSigners   :: TrustedSigners
    } deriving (Eq, Read, Show)

-- | 'StreamingDistributionSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsAliases' @::@ 'Aliases'
--
-- * 'sdsComment' @::@ 'Text'
--
-- * 'sdsDomainName' @::@ 'Text'
--
-- * 'sdsEnabled' @::@ 'Bool'
--
-- * 'sdsId' @::@ 'Text'
--
-- * 'sdsLastModifiedTime' @::@ 'UTCTime'
--
-- * 'sdsPriceClass' @::@ 'PriceClass'
--
-- * 'sdsS3Origin' @::@ 'S3Origin'
--
-- * 'sdsStatus' @::@ 'Text'
--
-- * 'sdsTrustedSigners' @::@ 'TrustedSigners'
--
streamingDistributionSummary :: Text -- ^ 'sdsId'
                             -> Text -- ^ 'sdsStatus'
                             -> UTCTime -- ^ 'sdsLastModifiedTime'
                             -> Text -- ^ 'sdsDomainName'
                             -> S3Origin -- ^ 'sdsS3Origin'
                             -> Aliases -- ^ 'sdsAliases'
                             -> TrustedSigners -- ^ 'sdsTrustedSigners'
                             -> Text -- ^ 'sdsComment'
                             -> PriceClass -- ^ 'sdsPriceClass'
                             -> Bool -- ^ 'sdsEnabled'
                             -> StreamingDistributionSummary
streamingDistributionSummary p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 = StreamingDistributionSummary
    { _sdsId               = p1
    , _sdsStatus           = p2
    , _sdsLastModifiedTime = withIso _Time (const id) p3
    , _sdsDomainName       = p4
    , _sdsS3Origin         = p5
    , _sdsAliases          = p6
    , _sdsTrustedSigners   = p7
    , _sdsComment          = p8
    , _sdsPriceClass       = p9
    , _sdsEnabled          = p10
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary Aliases
sdsAliases = lens _sdsAliases (\s a -> s { _sdsAliases = a })

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary Text
sdsComment = lens _sdsComment (\s a -> s { _sdsComment = a })

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
sdsDomainName :: Lens' StreamingDistributionSummary Text
sdsDomainName = lens _sdsDomainName (\s a -> s { _sdsDomainName = a })

-- | Whether the distribution is enabled to accept end user requests for content.
sdsEnabled :: Lens' StreamingDistributionSummary Bool
sdsEnabled = lens _sdsEnabled (\s a -> s { _sdsEnabled = a })

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
sdsId :: Lens' StreamingDistributionSummary Text
sdsId = lens _sdsId (\s a -> s { _sdsId = a })

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary UTCTime
sdsLastModifiedTime =
    lens _sdsLastModifiedTime (\s a -> s { _sdsLastModifiedTime = a })
        . _Time

sdsPriceClass :: Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = lens _sdsPriceClass (\s a -> s { _sdsPriceClass = a })

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = lens _sdsS3Origin (\s a -> s { _sdsS3Origin = a })

-- | Indicates the current status of the distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary Text
sdsStatus = lens _sdsStatus (\s a -> s { _sdsStatus = a })

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for Quantity.
-- Omit Items. To add, change, or remove one or more trusted signers, change
-- Enabled to true (if it's currently false), change Quantity as applicable, and
-- specify all of the trusted signers that you want to include in the updated
-- distribution.
sdsTrustedSigners :: Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners =
    lens _sdsTrustedSigners (\s a -> s { _sdsTrustedSigners = a })

instance FromXML StreamingDistributionSummary where
    parseXML x = StreamingDistributionSummary
        <$> x .@  "Aliases"
        <*> x .@  "Comment"
        <*> x .@  "DomainName"
        <*> x .@  "Enabled"
        <*> x .@  "Id"
        <*> x .@  "LastModifiedTime"
        <*> x .@  "PriceClass"
        <*> x .@  "S3Origin"
        <*> x .@  "Status"
        <*> x .@  "TrustedSigners"

instance ToXML StreamingDistributionSummary where
    toXML StreamingDistributionSummary{..} = nodes "StreamingDistributionSummary"
        [ "Id"               =@ _sdsId
        , "Status"           =@ _sdsStatus
        , "LastModifiedTime" =@ _sdsLastModifiedTime
        , "DomainName"       =@ _sdsDomainName
        , "S3Origin"         =@ _sdsS3Origin
        , "Aliases"          =@ _sdsAliases
        , "TrustedSigners"   =@ _sdsTrustedSigners
        , "Comment"          =@ _sdsComment
        , "PriceClass"       =@ _sdsPriceClass
        , "Enabled"          =@ _sdsEnabled
        ]

data CustomOriginConfig = CustomOriginConfig
    { _cocHTTPPort             :: Int
    , _cocHTTPSPort            :: Int
    , _cocOriginProtocolPolicy :: OriginProtocolPolicy
    } deriving (Eq, Read, Show)

-- | 'CustomOriginConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cocHTTPPort' @::@ 'Int'
--
-- * 'cocHTTPSPort' @::@ 'Int'
--
-- * 'cocOriginProtocolPolicy' @::@ 'OriginProtocolPolicy'
--
customOriginConfig :: Int -- ^ 'cocHTTPPort'
                   -> Int -- ^ 'cocHTTPSPort'
                   -> OriginProtocolPolicy -- ^ 'cocOriginProtocolPolicy'
                   -> CustomOriginConfig
customOriginConfig p1 p2 p3 = CustomOriginConfig
    { _cocHTTPPort             = p1
    , _cocHTTPSPort            = p2
    , _cocOriginProtocolPolicy = p3
    }

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\s a -> s { _cocHTTPPort = a })

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\s a -> s { _cocHTTPSPort = a })

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy =
    lens _cocOriginProtocolPolicy (\s a -> s { _cocOriginProtocolPolicy = a })

instance FromXML CustomOriginConfig where
    parseXML x = CustomOriginConfig
        <$> x .@  "HTTPPort"
        <*> x .@  "HTTPSPort"
        <*> x .@  "OriginProtocolPolicy"

instance ToXML CustomOriginConfig where
    toXML CustomOriginConfig{..} = nodes "CustomOriginConfig"
        [ "HTTPPort"             =@ _cocHTTPPort
        , "HTTPSPort"            =@ _cocHTTPSPort
        , "OriginProtocolPolicy" =@ _cocOriginProtocolPolicy
        ]

data Aliases = Aliases
    { _aItems    :: List "CNAME" Text
    , _aQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'Aliases' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aItems' @::@ ['Text']
--
-- * 'aQuantity' @::@ 'Int'
--
aliases :: Int -- ^ 'aQuantity'
        -> Aliases
aliases p1 = Aliases
    { _aQuantity = p1
    , _aItems    = mempty
    }

-- | Optional: A complex type that contains CNAME elements, if any, for this
-- distribution. If Quantity is 0, you can omit Items.
aItems :: Lens' Aliases [Text]
aItems = lens _aItems (\s a -> s { _aItems = a }) . _List

-- | The number of CNAMEs, if any, for this distribution.
aQuantity :: Lens' Aliases Int
aQuantity = lens _aQuantity (\s a -> s { _aQuantity = a })

instance FromXML Aliases where
    parseXML x = Aliases
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML Aliases where
    toXML Aliases{..} = nodes "Aliases"
        [ "Quantity" =@ _aQuantity
        , "Items"    =@ _aItems
        ]

data InvalidationBatch = InvalidationBatch
    { _ibCallerReference :: Text
    , _ibPaths           :: Paths
    } deriving (Eq, Read, Show)

-- | 'InvalidationBatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibCallerReference' @::@ 'Text'
--
-- * 'ibPaths' @::@ 'Paths'
--
invalidationBatch :: Paths -- ^ 'ibPaths'
                  -> Text -- ^ 'ibCallerReference'
                  -> InvalidationBatch
invalidationBatch p1 p2 = InvalidationBatch
    { _ibPaths           = p1
    , _ibCallerReference = p2
    }

-- | A unique name that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the Path object), a new
-- distribution is created. If the CallerReference is a value you already sent
-- in a previous request to create an invalidation batch, and the content of
-- each Path element is identical to the original request, the response includes
-- the same information returned to the original request. If the CallerReference
-- is a value you already sent in a previous request to create a distribution
-- but the content of any Path is different from the original request,
-- CloudFront returns an InvalidationBatchAlreadyExists error.
ibCallerReference :: Lens' InvalidationBatch Text
ibCallerReference =
    lens _ibCallerReference (\s a -> s { _ibCallerReference = a })

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
-- URL encode any other characters in the path, or CloudFront will not
-- invalidate the old version of the updated object.
ibPaths :: Lens' InvalidationBatch Paths
ibPaths = lens _ibPaths (\s a -> s { _ibPaths = a })

instance FromXML InvalidationBatch where
    parseXML x = InvalidationBatch
        <$> x .@  "CallerReference"
        <*> x .@  "Paths"

instance ToXML InvalidationBatch where
    toXML InvalidationBatch{..} = nodes "InvalidationBatch"
        [ "Paths"           =@ _ibPaths
        , "CallerReference" =@ _ibCallerReference
        ]

data InvalidationSummary = InvalidationSummary
    { _isCreateTime :: ISO8601
    , _isId         :: Text
    , _isStatus     :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'InvalidationSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isCreateTime' @::@ 'UTCTime'
--
-- * 'isId' @::@ 'Text'
--
-- * 'isStatus' @::@ 'Text'
--
invalidationSummary :: Text -- ^ 'isId'
                    -> UTCTime -- ^ 'isCreateTime'
                    -> Text -- ^ 'isStatus'
                    -> InvalidationSummary
invalidationSummary p1 p2 p3 = InvalidationSummary
    { _isId         = p1
    , _isCreateTime = withIso _Time (const id) p2
    , _isStatus     = p3
    }

isCreateTime :: Lens' InvalidationSummary UTCTime
isCreateTime = lens _isCreateTime (\s a -> s { _isCreateTime = a }) . _Time

-- | The unique ID for an invalidation request.
isId :: Lens' InvalidationSummary Text
isId = lens _isId (\s a -> s { _isId = a })

-- | The status of an invalidation request.
isStatus :: Lens' InvalidationSummary Text
isStatus = lens _isStatus (\s a -> s { _isStatus = a })

instance FromXML InvalidationSummary where
    parseXML x = InvalidationSummary
        <$> x .@  "CreateTime"
        <*> x .@  "Id"
        <*> x .@  "Status"

instance ToXML InvalidationSummary where
    toXML InvalidationSummary{..} = nodes "InvalidationSummary"
        [ "Id"         =@ _isId
        , "CreateTime" =@ _isCreateTime
        , "Status"     =@ _isStatus
        ]

data DistributionConfig = DistributionConfig
    { _dcAliases              :: Maybe Aliases
    , _dcCacheBehaviors       :: Maybe CacheBehaviors
    , _dcCallerReference      :: Text
    , _dcComment              :: Text
    , _dcCustomErrorResponses :: Maybe CustomErrorResponses
    , _dcDefaultCacheBehavior :: DefaultCacheBehavior
    , _dcDefaultRootObject    :: Maybe Text
    , _dcEnabled              :: Bool
    , _dcLogging              :: Maybe LoggingConfig
    , _dcOrigins              :: Origins
    , _dcPriceClass           :: Maybe PriceClass
    , _dcRestrictions         :: Maybe Restrictions
    , _dcViewerCertificate    :: Maybe ViewerCertificate
    } deriving (Eq, Read, Show)

-- | 'DistributionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcAliases' @::@ 'Maybe' 'Aliases'
--
-- * 'dcCacheBehaviors' @::@ 'Maybe' 'CacheBehaviors'
--
-- * 'dcCallerReference' @::@ 'Text'
--
-- * 'dcComment' @::@ 'Text'
--
-- * 'dcCustomErrorResponses' @::@ 'Maybe' 'CustomErrorResponses'
--
-- * 'dcDefaultCacheBehavior' @::@ 'DefaultCacheBehavior'
--
-- * 'dcDefaultRootObject' @::@ 'Maybe' 'Text'
--
-- * 'dcEnabled' @::@ 'Bool'
--
-- * 'dcLogging' @::@ 'Maybe' 'LoggingConfig'
--
-- * 'dcOrigins' @::@ 'Origins'
--
-- * 'dcPriceClass' @::@ 'Maybe' 'PriceClass'
--
-- * 'dcRestrictions' @::@ 'Maybe' 'Restrictions'
--
-- * 'dcViewerCertificate' @::@ 'Maybe' 'ViewerCertificate'
--
distributionConfig :: Text -- ^ 'dcCallerReference'
                   -> Origins -- ^ 'dcOrigins'
                   -> DefaultCacheBehavior -- ^ 'dcDefaultCacheBehavior'
                   -> Text -- ^ 'dcComment'
                   -> Bool -- ^ 'dcEnabled'
                   -> DistributionConfig
distributionConfig p1 p2 p3 p4 p5 = DistributionConfig
    { _dcCallerReference      = p1
    , _dcOrigins              = p2
    , _dcDefaultCacheBehavior = p3
    , _dcComment              = p4
    , _dcEnabled              = p5
    , _dcAliases              = Nothing
    , _dcDefaultRootObject    = Nothing
    , _dcCacheBehaviors       = Nothing
    , _dcCustomErrorResponses = Nothing
    , _dcLogging              = Nothing
    , _dcPriceClass           = Nothing
    , _dcViewerCertificate    = Nothing
    , _dcRestrictions         = Nothing
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Maybe Aliases)
dcAliases = lens _dcAliases (\s a -> s { _dcAliases = a })

-- | A complex type that contains zero or more CacheBehavior elements.
dcCacheBehaviors :: Lens' DistributionConfig (Maybe CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\s a -> s { _dcCacheBehaviors = a })

-- | A unique number that ensures the request can't be replayed. If the
-- CallerReference is new (no matter the content of the DistributionConfig
-- object), a new distribution is created. If the CallerReference is a value you
-- already sent in a previous request to create a distribution, and the content
-- of the DistributionConfig is identical to the original request (ignoring
-- white space), the response includes the same information returned to the
-- original request. If the CallerReference is a value you already sent in a
-- previous request to create a distribution but the content of the
-- DistributionConfig is different from the original request, CloudFront returns
-- a DistributionAlreadyExists error.
dcCallerReference :: Lens' DistributionConfig Text
dcCallerReference =
    lens _dcCallerReference (\s a -> s { _dcCallerReference = a })

-- | Any comments you want to include about the distribution.
dcComment :: Lens' DistributionConfig Text
dcComment = lens _dcComment (\s a -> s { _dcComment = a })

-- | A complex type that contains zero or more CustomErrorResponse elements.
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses =
    lens _dcCustomErrorResponses (\s a -> s { _dcCustomErrorResponses = a })

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values of
-- PathPattern in CacheBehavior elements.You must create exactly one default
-- cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior =
    lens _dcDefaultCacheBehavior (\s a -> s { _dcDefaultCacheBehavior = a })

-- | The object that you want CloudFront to return (for example, index.html) when
-- an end user requests the root URL for your distribution
-- (http://www.example.com) instead of an object in your distribution
-- (http://www.example.com/index.html). Specifying a default root object avoids
-- exposing the contents of your distribution. If you don't want to specify a
-- default root object when you create a distribution, include an empty
-- DefaultRootObject element. To delete the default root object from an existing
-- distribution, update the distribution configuration and include an empty
-- DefaultRootObject element. To replace the default root object, update the
-- distribution configuration and specify the new object.
dcDefaultRootObject :: Lens' DistributionConfig (Maybe Text)
dcDefaultRootObject =
    lens _dcDefaultRootObject (\s a -> s { _dcDefaultRootObject = a })

-- | Whether the distribution is enabled to accept end user requests for content.
dcEnabled :: Lens' DistributionConfig Bool
dcEnabled = lens _dcEnabled (\s a -> s { _dcEnabled = a })

-- | A complex type that controls whether access logs are written for the
-- distribution.
dcLogging :: Lens' DistributionConfig (Maybe LoggingConfig)
dcLogging = lens _dcLogging (\s a -> s { _dcLogging = a })

-- | A complex type that contains information about origins for this distribution.
dcOrigins :: Lens' DistributionConfig Origins
dcOrigins = lens _dcOrigins (\s a -> s { _dcOrigins = a })

-- | A complex type that contains information about price class for this
-- distribution.
dcPriceClass :: Lens' DistributionConfig (Maybe PriceClass)
dcPriceClass = lens _dcPriceClass (\s a -> s { _dcPriceClass = a })

dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\s a -> s { _dcRestrictions = a })

dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate =
    lens _dcViewerCertificate (\s a -> s { _dcViewerCertificate = a })

instance FromXML DistributionConfig where
    parseXML x = DistributionConfig
        <$> x .@? "Aliases"
        <*> x .@? "CacheBehaviors"
        <*> x .@  "CallerReference"
        <*> x .@  "Comment"
        <*> x .@? "CustomErrorResponses"
        <*> x .@  "DefaultCacheBehavior"
        <*> x .@? "DefaultRootObject"
        <*> x .@  "Enabled"
        <*> x .@? "Logging"
        <*> x .@  "Origins"
        <*> x .@? "PriceClass"
        <*> x .@? "Restrictions"
        <*> x .@? "ViewerCertificate"

instance ToXMLRoot DistributionConfig where
    toXMLRoot DistributionConfig{..} = namespaced ns "DistributionConfig"
        [ "CallerReference"      =@ _dcCallerReference
        , "Aliases"              =@ _dcAliases
        , "DefaultRootObject"    =@ _dcDefaultRootObject
        , "Origins"              =@ _dcOrigins
        , "DefaultCacheBehavior" =@ _dcDefaultCacheBehavior
        , "CacheBehaviors"       =@ _dcCacheBehaviors
        , "CustomErrorResponses" =@ _dcCustomErrorResponses
        , "Comment"              =@ _dcComment
        , "Logging"              =@ _dcLogging
        , "PriceClass"           =@ _dcPriceClass
        , "Enabled"              =@ _dcEnabled
        , "ViewerCertificate"    =@ _dcViewerCertificate
        , "Restrictions"         =@ _dcRestrictions
        ]

instance ToXML DistributionConfig

data CacheBehavior = CacheBehavior
    { _cbAllowedMethods       :: Maybe AllowedMethods
    , _cbForwardedValues      :: ForwardedValues
    , _cbMinTTL               :: Integer
    , _cbPathPattern          :: Text
    , _cbSmoothStreaming      :: Maybe Bool
    , _cbTargetOriginId       :: Text
    , _cbTrustedSigners       :: TrustedSigners
    , _cbViewerProtocolPolicy :: ViewerProtocolPolicy
    } deriving (Eq, Read, Show)

-- | 'CacheBehavior' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbAllowedMethods' @::@ 'Maybe' 'AllowedMethods'
--
-- * 'cbForwardedValues' @::@ 'ForwardedValues'
--
-- * 'cbMinTTL' @::@ 'Integer'
--
-- * 'cbPathPattern' @::@ 'Text'
--
-- * 'cbSmoothStreaming' @::@ 'Maybe' 'Bool'
--
-- * 'cbTargetOriginId' @::@ 'Text'
--
-- * 'cbTrustedSigners' @::@ 'TrustedSigners'
--
-- * 'cbViewerProtocolPolicy' @::@ 'ViewerProtocolPolicy'
--
cacheBehavior :: Text -- ^ 'cbPathPattern'
              -> Text -- ^ 'cbTargetOriginId'
              -> ForwardedValues -- ^ 'cbForwardedValues'
              -> TrustedSigners -- ^ 'cbTrustedSigners'
              -> ViewerProtocolPolicy -- ^ 'cbViewerProtocolPolicy'
              -> Integer -- ^ 'cbMinTTL'
              -> CacheBehavior
cacheBehavior p1 p2 p3 p4 p5 p6 = CacheBehavior
    { _cbPathPattern          = p1
    , _cbTargetOriginId       = p2
    , _cbForwardedValues      = p3
    , _cbTrustedSigners       = p4
    , _cbViewerProtocolPolicy = p5
    , _cbMinTTL               = p6
    , _cbAllowedMethods       = Nothing
    , _cbSmoothStreaming      = Nothing
    }

cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\s a -> s { _cbAllowedMethods = a })

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
cbForwardedValues :: Lens' CacheBehavior ForwardedValues
cbForwardedValues =
    lens _cbForwardedValues (\s a -> s { _cbForwardedValues = a })

-- | The minimum amount of time that you want objects to stay in CloudFront caches
-- before CloudFront queries your origin to see whether the object has been
-- updated.You can specify a value from 0 to 3,153,600,000 seconds (100 years).
cbMinTTL :: Lens' CacheBehavior Integer
cbMinTTL = lens _cbMinTTL (\s a -> s { _cbMinTTL = a })

-- | The pattern (for example, images/*.jpg) that specifies which requests you
-- want this cache behavior to apply to. When CloudFront receives an end-user
-- request, the requested path is compared with path patterns in the order in
-- which cache behaviors are listed in the distribution. The path pattern for
-- the default cache behavior is * and cannot be changed. If the request for an
-- object does not match the path pattern for any cache behaviors, CloudFront
-- applies the behavior in the default cache behavior.
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\s a -> s { _cbPathPattern = a })

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming =
    lens _cbSmoothStreaming (\s a -> s { _cbSmoothStreaming = a })

-- | The value of ID for the origin that you want CloudFront to route requests to
-- when a request matches the path pattern either for a cache behavior or for
-- the default cache behavior.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\s a -> s { _cbTargetOriginId = a })

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for Quantity.
-- Omit Items. To add, change, or remove one or more trusted signers, change
-- Enabled to true (if it's currently false), change Quantity as applicable, and
-- specify all of the trusted signers that you want to include in the updated
-- distribution.
cbTrustedSigners :: Lens' CacheBehavior TrustedSigners
cbTrustedSigners = lens _cbTrustedSigners (\s a -> s { _cbTrustedSigners = a })

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to use
-- any available protocol, specify allow-all. If you want CloudFront to require
-- HTTPS, specify https. If you want CloudFront to respond to an HTTP request
-- with an HTTP status code of 301 (Moved Permanently) and the HTTPS URL,
-- specify redirect-to-https. The viewer then resubmits the request using the
-- HTTPS URL.
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy =
    lens _cbViewerProtocolPolicy (\s a -> s { _cbViewerProtocolPolicy = a })

instance FromXML CacheBehavior where
    parseXML x = CacheBehavior
        <$> x .@? "AllowedMethods"
        <*> x .@  "ForwardedValues"
        <*> x .@  "MinTTL"
        <*> x .@  "PathPattern"
        <*> x .@? "SmoothStreaming"
        <*> x .@  "TargetOriginId"
        <*> x .@  "TrustedSigners"
        <*> x .@  "ViewerProtocolPolicy"

instance ToXML CacheBehavior where
    toXML CacheBehavior{..} = nodes "CacheBehavior"
        [ "PathPattern"          =@ _cbPathPattern
        , "TargetOriginId"       =@ _cbTargetOriginId
        , "ForwardedValues"      =@ _cbForwardedValues
        , "TrustedSigners"       =@ _cbTrustedSigners
        , "ViewerProtocolPolicy" =@ _cbViewerProtocolPolicy
        , "MinTTL"               =@ _cbMinTTL
        , "AllowedMethods"       =@ _cbAllowedMethods
        , "SmoothStreaming"      =@ _cbSmoothStreaming
        ]

data DistributionList = DistributionList
    { _dlIsTruncated :: Bool
    , _dlItems       :: List "DistributionSummary" DistributionSummary
    , _dlMarker      :: Text
    , _dlMaxItems    :: Int
    , _dlNextMarker  :: Maybe Text
    , _dlQuantity    :: Int
    } deriving (Eq, Read, Show)

-- | 'DistributionList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlIsTruncated' @::@ 'Bool'
--
-- * 'dlItems' @::@ ['DistributionSummary']
--
-- * 'dlMarker' @::@ 'Text'
--
-- * 'dlMaxItems' @::@ 'Int'
--
-- * 'dlNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'dlQuantity' @::@ 'Int'
--
distributionList :: Text -- ^ 'dlMarker'
                 -> Int -- ^ 'dlMaxItems'
                 -> Bool -- ^ 'dlIsTruncated'
                 -> Int -- ^ 'dlQuantity'
                 -> DistributionList
distributionList p1 p2 p3 p4 = DistributionList
    { _dlMarker      = p1
    , _dlMaxItems    = p2
    , _dlIsTruncated = p3
    , _dlQuantity    = p4
    , _dlNextMarker  = Nothing
    , _dlItems       = mempty
    }

-- | A flag that indicates whether more distributions remain to be listed. If your
-- results were truncated, you can make a follow-up pagination request using the
-- Marker request parameter to retrieve more distributions in the list.
dlIsTruncated :: Lens' DistributionList Bool
dlIsTruncated = lens _dlIsTruncated (\s a -> s { _dlIsTruncated = a })

-- | A complex type that contains one DistributionSummary element for each
-- distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList [DistributionSummary]
dlItems = lens _dlItems (\s a -> s { _dlItems = a }) . _List

-- | The value you provided for the Marker request parameter.
dlMarker :: Lens' DistributionList Text
dlMarker = lens _dlMarker (\s a -> s { _dlMarker = a })

-- | The value you provided for the MaxItems request parameter.
dlMaxItems :: Lens' DistributionList Int
dlMaxItems = lens _dlMaxItems (\s a -> s { _dlMaxItems = a })

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\s a -> s { _dlNextMarker = a })

-- | The number of distributions that were created by the current AWS account.
dlQuantity :: Lens' DistributionList Int
dlQuantity = lens _dlQuantity (\s a -> s { _dlQuantity = a })

instance FromXML DistributionList where
    parseXML x = DistributionList
        <$> x .@  "IsTruncated"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Marker"
        <*> x .@  "MaxItems"
        <*> x .@? "NextMarker"
        <*> x .@  "Quantity"

instance ToXML DistributionList where
    toXML DistributionList{..} = nodes "DistributionList"
        [ "Marker"      =@ _dlMarker
        , "NextMarker"  =@ _dlNextMarker
        , "MaxItems"    =@ _dlMaxItems
        , "IsTruncated" =@ _dlIsTruncated
        , "Quantity"    =@ _dlQuantity
        , "Items"       =@ _dlItems
        ]

data KeyPairIds = KeyPairIds
    { _kpiItems    :: List "KeyPairId" Text
    , _kpiQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'KeyPairIds' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpiItems' @::@ ['Text']
--
-- * 'kpiQuantity' @::@ 'Int'
--
keyPairIds :: Int -- ^ 'kpiQuantity'
           -> KeyPairIds
keyPairIds p1 = KeyPairIds
    { _kpiQuantity = p1
    , _kpiItems    = mempty
    }

-- | A complex type that lists the active CloudFront key pairs, if any, that are
-- associated with AwsAccountNumber.
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\s a -> s { _kpiItems = a }) . _List

-- | The number of active CloudFront key pairs for AwsAccountNumber.
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\s a -> s { _kpiQuantity = a })

instance FromXML KeyPairIds where
    parseXML x = KeyPairIds
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML KeyPairIds where
    toXML KeyPairIds{..} = nodes "KeyPairIds"
        [ "Quantity" =@ _kpiQuantity
        , "Items"    =@ _kpiItems
        ]

data PriceClass
    = PriceClass100 -- ^ PriceClass_100
    | PriceClass200 -- ^ PriceClass_200
    | PriceClassAll -- ^ PriceClass_All
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable PriceClass

instance FromText PriceClass where
    parser = takeLowerText >>= \case
        "priceclass_100" -> pure PriceClass100
        "priceclass_200" -> pure PriceClass200
        "priceclass_all" -> pure PriceClassAll
        e                -> fail $
            "Failure parsing PriceClass from " ++ show e

instance ToText PriceClass where
    toText = \case
        PriceClass100 -> "PriceClass_100"
        PriceClass200 -> "PriceClass_200"
        PriceClassAll -> "PriceClass_All"

instance ToByteString PriceClass
instance ToHeader     PriceClass
instance ToQuery      PriceClass

instance FromXML PriceClass where
    parseXML = parseXMLText "PriceClass"

instance ToXML PriceClass where
    toXML = toXMLText

data CustomErrorResponses = CustomErrorResponses
    { _cerItems    :: List "CustomErrorResponse" CustomErrorResponse
    , _cerQuantity :: Int
    } deriving (Eq, Read, Show)

-- | 'CustomErrorResponses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerItems' @::@ ['CustomErrorResponse']
--
-- * 'cerQuantity' @::@ 'Int'
--
customErrorResponses :: Int -- ^ 'cerQuantity'
                     -> CustomErrorResponses
customErrorResponses p1 = CustomErrorResponses
    { _cerQuantity = p1
    , _cerItems    = mempty
    }

-- | Optional: A complex type that contains custom error responses for this
-- distribution. If Quantity is 0, you can omit Items.
cerItems :: Lens' CustomErrorResponses [CustomErrorResponse]
cerItems = lens _cerItems (\s a -> s { _cerItems = a }) . _List

-- | The number of custom error responses for this distribution.
cerQuantity :: Lens' CustomErrorResponses Int
cerQuantity = lens _cerQuantity (\s a -> s { _cerQuantity = a })

instance FromXML CustomErrorResponses where
    parseXML x = CustomErrorResponses
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML CustomErrorResponses where
    toXML CustomErrorResponses{..} = nodes "CustomErrorResponses"
        [ "Quantity" =@ _cerQuantity
        , "Items"    =@ _cerItems
        ]

newtype S3OriginConfig = S3OriginConfig
    { _socOriginAccessIdentity :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'S3OriginConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'socOriginAccessIdentity' @::@ 'Text'
--
s3OriginConfig :: Text -- ^ 'socOriginAccessIdentity'
               -> S3OriginConfig
s3OriginConfig p1 = S3OriginConfig
    { _socOriginAccessIdentity = p1
    }

-- | The CloudFront origin access identity to associate with the origin. Use an
-- origin access identity to configure the origin so that end users can only
-- access objects in an Amazon S3 bucket through CloudFront. If you want end
-- users to be able to access objects using either the CloudFront URL or the
-- Amazon S3 URL, specify an empty OriginAccessIdentity element. To delete the
-- origin access identity from an existing distribution, update the distribution
-- configuration and include an empty OriginAccessIdentity element. To replace
-- the origin access identity, update the distribution configuration and specify
-- the new origin access identity. Use the format
-- origin-access-identity/cloudfront/Id where Id is the value that CloudFront
-- returned in the Id element when you created the origin access identity.
socOriginAccessIdentity :: Lens' S3OriginConfig Text
socOriginAccessIdentity =
    lens _socOriginAccessIdentity (\s a -> s { _socOriginAccessIdentity = a })

instance FromXML S3OriginConfig where
    parseXML x = S3OriginConfig
        <$> x .@  "OriginAccessIdentity"

instance ToXML S3OriginConfig where
    toXML S3OriginConfig{..} = nodes "S3OriginConfig"
        [ "OriginAccessIdentity" =@ _socOriginAccessIdentity
        ]

data GeoRestriction = GeoRestriction
    { _grItems           :: List "Location" Text
    , _grQuantity        :: Int
    , _grRestrictionType :: GeoRestrictionType
    } deriving (Eq, Read, Show)

-- | 'GeoRestriction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grItems' @::@ ['Text']
--
-- * 'grQuantity' @::@ 'Int'
--
-- * 'grRestrictionType' @::@ 'GeoRestrictionType'
--
geoRestriction :: GeoRestrictionType -- ^ 'grRestrictionType'
               -> Int -- ^ 'grQuantity'
               -> GeoRestriction
geoRestriction p1 p2 = GeoRestriction
    { _grRestrictionType = p1
    , _grQuantity        = p2
    , _grItems           = mempty
    }

-- | A complex type that contains a Location element for each country in which you
-- want CloudFront either to distribute your content (whitelist) or not
-- distribute your content (blacklist). The Location element is a two-letter,
-- uppercase country code for a country that you want to include in your
-- blacklist or whitelist. Include one Location element for each country.
-- CloudFront and MaxMind both use ISO 3166 country codes. For the current list
-- of countries and the corresponding codes, see ISO 3166-1-alpha-2 code on the
-- International Organization for Standardization website. You can also refer to
-- the country list in the CloudFront console, which includes both country names
-- and codes.
grItems :: Lens' GeoRestriction [Text]
grItems = lens _grItems (\s a -> s { _grItems = a }) . _List

-- | When geo restriction is enabled, this is the number of countries in your
-- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is 0, and
-- you can omit Items.
grQuantity :: Lens' GeoRestriction Int
grQuantity = lens _grQuantity (\s a -> s { _grQuantity = a })

-- | The method that you want to use to restrict distribution of your content by
-- country: - none: No geo restriction is enabled, meaning access to content is
-- not restricted by client geo location. - blacklist: The Location elements
-- specify the countries in which you do not want CloudFront to distribute your
-- content. - whitelist: The Location elements specify the countries in which
-- you want CloudFront to distribute your content.
grRestrictionType :: Lens' GeoRestriction GeoRestrictionType
grRestrictionType =
    lens _grRestrictionType (\s a -> s { _grRestrictionType = a })

instance FromXML GeoRestriction where
    parseXML x = GeoRestriction
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"
        <*> x .@  "RestrictionType"

instance ToXML GeoRestriction where
    toXML GeoRestriction{..} = nodes "GeoRestriction"
        [ "RestrictionType" =@ _grRestrictionType
        , "Quantity"        =@ _grQuantity
        , "Items"           =@ _grItems
        ]

data S3Origin = S3Origin
    { _soDomainName           :: Text
    , _soOriginAccessIdentity :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'S3Origin' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'soDomainName' @::@ 'Text'
--
-- * 'soOriginAccessIdentity' @::@ 'Text'
--
s3Origin :: Text -- ^ 'soDomainName'
         -> Text -- ^ 'soOriginAccessIdentity'
         -> S3Origin
s3Origin p1 p2 = S3Origin
    { _soDomainName           = p1
    , _soOriginAccessIdentity = p2
    }

-- | The DNS name of the S3 origin.
soDomainName :: Lens' S3Origin Text
soDomainName = lens _soDomainName (\s a -> s { _soDomainName = a })

-- | Your S3 origin's origin access identity.
soOriginAccessIdentity :: Lens' S3Origin Text
soOriginAccessIdentity =
    lens _soOriginAccessIdentity (\s a -> s { _soOriginAccessIdentity = a })

instance FromXML S3Origin where
    parseXML x = S3Origin
        <$> x .@  "DomainName"
        <*> x .@  "OriginAccessIdentity"

instance ToXML S3Origin where
    toXML S3Origin{..} = nodes "S3Origin"
        [ "DomainName"           =@ _soDomainName
        , "OriginAccessIdentity" =@ _soOriginAccessIdentity
        ]

data Headers = Headers
    { _hItems    :: List "Name" Text
    , _hQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'Headers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hItems' @::@ ['Text']
--
-- * 'hQuantity' @::@ 'Int'
--
headers :: Int -- ^ 'hQuantity'
        -> Headers
headers p1 = Headers
    { _hQuantity = p1
    , _hItems    = mempty
    }

-- | Optional: A complex type that contains a Name element for each header that
-- you want CloudFront to forward to the origin and to vary on for this cache
-- behavior. If Quantity is 0, omit Items.
hItems :: Lens' Headers [Text]
hItems = lens _hItems (\s a -> s { _hItems = a }) . _List

-- | The number of different headers that you want CloudFront to forward to the
-- origin and to vary on for this cache behavior. The maximum number of headers
-- that you can specify by name is 10. If you want CloudFront to forward all
-- headers to the origin and vary on all of them, specify 1 for Quantity and *
-- for Name. If you don't want CloudFront to forward any additional headers to
-- the origin or to vary on any headers, specify 0 for Quantity and omit Items.
hQuantity :: Lens' Headers Int
hQuantity = lens _hQuantity (\s a -> s { _hQuantity = a })

instance FromXML Headers where
    parseXML x = Headers
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML Headers where
    toXML Headers{..} = nodes "Headers"
        [ "Quantity" =@ _hQuantity
        , "Items"    =@ _hItems
        ]

data CachedMethods = CachedMethods
    { _cmItems    :: List "Method" Method
    , _cmQuantity :: Int
    } deriving (Eq, Read, Show)

-- | 'CachedMethods' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmItems' @::@ ['Method']
--
-- * 'cmQuantity' @::@ 'Int'
--
cachedMethods :: Int -- ^ 'cmQuantity'
              -> CachedMethods
cachedMethods p1 = CachedMethods
    { _cmQuantity = p1
    , _cmItems    = mempty
    }

-- | A complex type that contains the HTTP methods that you want CloudFront to
-- cache responses to.
cmItems :: Lens' CachedMethods [Method]
cmItems = lens _cmItems (\s a -> s { _cmItems = a }) . _List

-- | The number of HTTP methods for which you want CloudFront to cache responses.
-- Valid values are 2 (for caching responses to GET and HEAD requests) and 3
-- (for caching responses to GET, HEAD, and OPTIONS requests).
cmQuantity :: Lens' CachedMethods Int
cmQuantity = lens _cmQuantity (\s a -> s { _cmQuantity = a })

instance FromXML CachedMethods where
    parseXML x = CachedMethods
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML CachedMethods where
    toXML CachedMethods{..} = nodes "CachedMethods"
        [ "Quantity" =@ _cmQuantity
        , "Items"    =@ _cmItems
        ]

data ViewerCertificate = ViewerCertificate
    { _vcCloudFrontDefaultCertificate :: Maybe Bool
    , _vcIAMCertificateId             :: Maybe Text
    , _vcMinimumProtocolVersion       :: Maybe MinimumProtocolVersion
    , _vcSSLSupportMethod             :: Maybe SSLSupportMethod
    } deriving (Eq, Read, Show)

-- | 'ViewerCertificate' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcCloudFrontDefaultCertificate' @::@ 'Maybe' 'Bool'
--
-- * 'vcIAMCertificateId' @::@ 'Maybe' 'Text'
--
-- * 'vcMinimumProtocolVersion' @::@ 'Maybe' 'MinimumProtocolVersion'
--
-- * 'vcSSLSupportMethod' @::@ 'Maybe' 'SSLSupportMethod'
--
viewerCertificate :: ViewerCertificate
viewerCertificate = ViewerCertificate
    { _vcIAMCertificateId             = Nothing
    , _vcCloudFrontDefaultCertificate = Nothing
    , _vcSSLSupportMethod             = Nothing
    , _vcMinimumProtocolVersion       = Nothing
    }

-- | If you want viewers to use HTTPS to request your objects and you're using the
-- CloudFront domain name of your distribution in your object URLs (for example,
-- https://d111111abcdef8.cloudfront.net/logo.jpg), set to true. Omit this value
-- if you are setting an IAMCertificateId.
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate =
    lens _vcCloudFrontDefaultCertificate
        (\s a -> s { _vcCloudFrontDefaultCertificate = a })

-- | If you want viewers to use HTTPS to request your objects and you're using an
-- alternate domain name in your object URLs (for example,
-- https://example.com/logo.jpg), specify the IAM certificate identifier of the
-- custom viewer certificate for this distribution. Specify either this value or
-- CloudFrontDefaultCertificate.
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId =
    lens _vcIAMCertificateId (\s a -> s { _vcIAMCertificateId = a })

-- | Specify the minimum version of the SSL protocol that you want CloudFront to
-- use, SSLv3 or TLSv1, for HTTPS connections. CloudFront will serve your
-- objects only to browsers or devices that support at least the SSL version
-- that you specify. The TLSv1 protocol is more secure, so we recommend that you
-- specify SSLv3 only if your users are using browsers or devices that don't
-- support TLSv1. If you're using a custom certificate (if you specify a value
-- for IAMCertificateId) and if you're using dedicated IP (if you specify vip
-- for SSLSupportMethod), you can choose SSLv3 or TLSv1 as the
-- MinimumProtocolVersion. If you're using a custom certificate (if you specify
-- a value for IAMCertificateId) and if you're using SNI (if you specify
-- sni-only for SSLSupportMethod), you must specify TLSv1 for
-- MinimumProtocolVersion.
vcMinimumProtocolVersion :: Lens' ViewerCertificate (Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion =
    lens _vcMinimumProtocolVersion
        (\s a -> s { _vcMinimumProtocolVersion = a })

-- | If you specify a value for IAMCertificateId, you must also specify how you
-- want CloudFront to serve HTTPS requests. Valid values are vip and sni-only.
-- If you specify vip, CloudFront uses dedicated IP addresses for your content
-- and can respond to HTTPS requests from any viewer. However, you must request
-- permission to use this feature, and you incur additional monthly charges. If
-- you specify sni-only, CloudFront can only respond to HTTPS requests from
-- viewers that support Server Name Indication (SNI). All modern browsers
-- support SNI, but some browsers still in use don't support SNI. Do not specify
-- a value for SSLSupportMethod if you specified true for
-- CloudFrontDefaultCertificate.
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod =
    lens _vcSSLSupportMethod (\s a -> s { _vcSSLSupportMethod = a })

instance FromXML ViewerCertificate where
    parseXML x = ViewerCertificate
        <$> x .@? "CloudFrontDefaultCertificate"
        <*> x .@? "IAMCertificateId"
        <*> x .@? "MinimumProtocolVersion"
        <*> x .@? "SSLSupportMethod"

instance ToXML ViewerCertificate where
    toXML ViewerCertificate{..} = nodes "ViewerCertificate"
        [ "IAMCertificateId"             =@ _vcIAMCertificateId
        , "CloudFrontDefaultCertificate" =@ _vcCloudFrontDefaultCertificate
        , "SSLSupportMethod"             =@ _vcSSLSupportMethod
        , "MinimumProtocolVersion"       =@ _vcMinimumProtocolVersion
        ]

newtype Restrictions = Restrictions
    { _rGeoRestriction :: GeoRestriction
    } deriving (Eq, Read, Show)

-- | 'Restrictions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rGeoRestriction' @::@ 'GeoRestriction'
--
restrictions :: GeoRestriction -- ^ 'rGeoRestriction'
             -> Restrictions
restrictions p1 = Restrictions
    { _rGeoRestriction = p1
    }

rGeoRestriction :: Lens' Restrictions GeoRestriction
rGeoRestriction = lens _rGeoRestriction (\s a -> s { _rGeoRestriction = a })

instance FromXML Restrictions where
    parseXML x = Restrictions
        <$> x .@  "GeoRestriction"

instance ToXML Restrictions where
    toXML Restrictions{..} = nodes "Restrictions"
        [ "GeoRestriction" =@ _rGeoRestriction
        ]

data Origins = Origins
    { _oItems    :: List1 "Origin" Origin
    , _oQuantity :: Int
    } deriving (Eq, Read, Show)

-- | 'Origins' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oItems' @::@ 'NonEmpty' 'Origin'
--
-- * 'oQuantity' @::@ 'Int'
--
origins :: Int -- ^ 'oQuantity'
        -> NonEmpty Origin -- ^ 'oItems'
        -> Origins
origins p1 p2 = Origins
    { _oQuantity = p1
    , _oItems    = withIso _List1 (const id) p2
    }

-- | A complex type that contains origins for this distribution.
oItems :: Lens' Origins (NonEmpty Origin)
oItems = lens _oItems (\s a -> s { _oItems = a }) . _List1

-- | The number of origins for this distribution.
oQuantity :: Lens' Origins Int
oQuantity = lens _oQuantity (\s a -> s { _oQuantity = a })

instance FromXML Origins where
    parseXML x = Origins
        <$> x .@  "Items"
        <*> x .@  "Quantity"

instance ToXML Origins where
    toXML Origins{..} = nodes "Origins"
        [ "Quantity" =@ _oQuantity
        , "Items"    =@ _oItems
        ]

data Method
    = Delete' -- ^ DELETE
    | Get     -- ^ GET
    | Head'   -- ^ HEAD
    | Options -- ^ OPTIONS
    | Patch   -- ^ PATCH
    | Post    -- ^ POST
    | Put     -- ^ PUT
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Method

instance FromText Method where
    parser = takeLowerText >>= \case
        "delete"  -> pure Delete'
        "get"     -> pure Get
        "head"    -> pure Head'
        "options" -> pure Options
        "patch"   -> pure Patch
        "post"    -> pure Post
        "put"     -> pure Put
        e         -> fail $
            "Failure parsing Method from " ++ show e

instance ToText Method where
    toText = \case
        Delete' -> "DELETE"
        Get     -> "GET"
        Head'   -> "HEAD"
        Options -> "OPTIONS"
        Patch   -> "PATCH"
        Post    -> "POST"
        Put     -> "PUT"

instance ToByteString Method
instance ToHeader     Method
instance ToQuery      Method

instance FromXML Method where
    parseXML = parseXMLText "Method"

instance ToXML Method where
    toXML = toXMLText

data MinimumProtocolVersion
    = SSLv3 -- ^ SSLv3
    | TLSv1 -- ^ TLSv1
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable MinimumProtocolVersion

instance FromText MinimumProtocolVersion where
    parser = takeLowerText >>= \case
        "sslv3" -> pure SSLv3
        "tlsv1" -> pure TLSv1
        e       -> fail $
            "Failure parsing MinimumProtocolVersion from " ++ show e

instance ToText MinimumProtocolVersion where
    toText = \case
        SSLv3 -> "SSLv3"
        TLSv1 -> "TLSv1"

instance ToByteString MinimumProtocolVersion
instance ToHeader     MinimumProtocolVersion
instance ToQuery      MinimumProtocolVersion

instance FromXML MinimumProtocolVersion where
    parseXML = parseXMLText "MinimumProtocolVersion"

instance ToXML MinimumProtocolVersion where
    toXML = toXMLText

data ForwardedValues = ForwardedValues
    { _fvCookies     :: CookiePreference
    , _fvHeaders     :: Maybe Headers
    , _fvQueryString :: Bool
    } deriving (Eq, Read, Show)

-- | 'ForwardedValues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fvCookies' @::@ 'CookiePreference'
--
-- * 'fvHeaders' @::@ 'Maybe' 'Headers'
--
-- * 'fvQueryString' @::@ 'Bool'
--
forwardedValues :: Bool -- ^ 'fvQueryString'
                -> CookiePreference -- ^ 'fvCookies'
                -> ForwardedValues
forwardedValues p1 p2 = ForwardedValues
    { _fvQueryString = p1
    , _fvCookies     = p2
    , _fvHeaders     = Nothing
    }

-- | A complex type that specifies how CloudFront handles cookies.
fvCookies :: Lens' ForwardedValues CookiePreference
fvCookies = lens _fvCookies (\s a -> s { _fvCookies = a })

-- | A complex type that specifies the Headers, if any, that you want CloudFront
-- to vary upon for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\s a -> s { _fvHeaders = a })

-- | Indicates whether you want CloudFront to forward query strings to the origin
-- that is associated with this cache behavior. If so, specify true; if not,
-- specify false.
fvQueryString :: Lens' ForwardedValues Bool
fvQueryString = lens _fvQueryString (\s a -> s { _fvQueryString = a })

instance FromXML ForwardedValues where
    parseXML x = ForwardedValues
        <$> x .@  "Cookies"
        <*> x .@? "Headers"
        <*> x .@  "QueryString"

instance ToXML ForwardedValues where
    toXML ForwardedValues{..} = nodes "ForwardedValues"
        [ "QueryString" =@ _fvQueryString
        , "Cookies"     =@ _fvCookies
        , "Headers"     =@ _fvHeaders
        ]

data TrustedSigners = TrustedSigners
    { _tsEnabled  :: Bool
    , _tsItems    :: List "AwsAccountNumber" Text
    , _tsQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'TrustedSigners' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tsEnabled' @::@ 'Bool'
--
-- * 'tsItems' @::@ ['Text']
--
-- * 'tsQuantity' @::@ 'Int'
--
trustedSigners :: Bool -- ^ 'tsEnabled'
               -> Int -- ^ 'tsQuantity'
               -> TrustedSigners
trustedSigners p1 p2 = TrustedSigners
    { _tsEnabled  = p1
    , _tsQuantity = p2
    , _tsItems    = mempty
    }

-- | Specifies whether you want to require end users to use signed URLs to access
-- the files specified by PathPattern and TargetOriginId.
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\s a -> s { _tsEnabled = a })

-- | Optional: A complex type that contains trusted signers for this cache
-- behavior. If Quantity is 0, you can omit Items.
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\s a -> s { _tsItems = a }) . _List

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\s a -> s { _tsQuantity = a })

instance FromXML TrustedSigners where
    parseXML x = TrustedSigners
        <$> x .@  "Enabled"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML TrustedSigners where
    toXML TrustedSigners{..} = nodes "TrustedSigners"
        [ "Enabled"  =@ _tsEnabled
        , "Quantity" =@ _tsQuantity
        , "Items"    =@ _tsItems
        ]

data ItemSelection
    = All       -- ^ all
    | None      -- ^ none
    | Whitelist -- ^ whitelist
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ItemSelection

instance FromText ItemSelection where
    parser = takeLowerText >>= \case
        "all"       -> pure All
        "none"      -> pure None
        "whitelist" -> pure Whitelist
        e           -> fail $
            "Failure parsing ItemSelection from " ++ show e

instance ToText ItemSelection where
    toText = \case
        All       -> "all"
        None      -> "none"
        Whitelist -> "whitelist"

instance ToByteString ItemSelection
instance ToHeader     ItemSelection
instance ToQuery      ItemSelection

instance FromXML ItemSelection where
    parseXML = parseXMLText "ItemSelection"

instance ToXML ItemSelection where
    toXML = toXMLText

data StreamingLoggingConfig = StreamingLoggingConfig
    { _slcBucket  :: Text
    , _slcEnabled :: Bool
    , _slcPrefix  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'StreamingLoggingConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slcBucket' @::@ 'Text'
--
-- * 'slcEnabled' @::@ 'Bool'
--
-- * 'slcPrefix' @::@ 'Text'
--
streamingLoggingConfig :: Bool -- ^ 'slcEnabled'
                       -> Text -- ^ 'slcBucket'
                       -> Text -- ^ 'slcPrefix'
                       -> StreamingLoggingConfig
streamingLoggingConfig p1 p2 p3 = StreamingLoggingConfig
    { _slcEnabled = p1
    , _slcBucket  = p2
    , _slcPrefix  = p3
    }

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
slcBucket :: Lens' StreamingLoggingConfig Text
slcBucket = lens _slcBucket (\s a -> s { _slcBucket = a })

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3
-- bucket. If you do not want to enable logging when you create a streaming
-- distribution or if you want to disable logging for an existing streaming
-- distribution, specify false for Enabled, and specify empty Bucket and Prefix
-- elements. If you specify false for Enabled but you specify values for Bucket
-- and Prefix, the values are automatically deleted.
slcEnabled :: Lens' StreamingLoggingConfig Bool
slcEnabled = lens _slcEnabled (\s a -> s { _slcEnabled = a })

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, myprefix/. If you
-- want to enable logging, but you do not want to specify a prefix, you still
-- must include an empty Prefix element in the Logging element.
slcPrefix :: Lens' StreamingLoggingConfig Text
slcPrefix = lens _slcPrefix (\s a -> s { _slcPrefix = a })

instance FromXML StreamingLoggingConfig where
    parseXML x = StreamingLoggingConfig
        <$> x .@  "Bucket"
        <*> x .@  "Enabled"
        <*> x .@  "Prefix"

instance ToXML StreamingLoggingConfig where
    toXML StreamingLoggingConfig{..} = nodes "StreamingLoggingConfig"
        [ "Enabled" =@ _slcEnabled
        , "Bucket"  =@ _slcBucket
        , "Prefix"  =@ _slcPrefix
        ]

data CookieNames = CookieNames
    { _cnItems    :: List "Name" Text
    , _cnQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'CookieNames' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnItems' @::@ ['Text']
--
-- * 'cnQuantity' @::@ 'Int'
--
cookieNames :: Int -- ^ 'cnQuantity'
            -> CookieNames
cookieNames p1 = CookieNames
    { _cnQuantity = p1
    , _cnItems    = mempty
    }

-- | Optional: A complex type that contains whitelisted cookies for this cache
-- behavior. If Quantity is 0, you can omit Items.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\s a -> s { _cnItems = a }) . _List

-- | The number of whitelisted cookies for this cache behavior.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\s a -> s { _cnQuantity = a })

instance FromXML CookieNames where
    parseXML x = CookieNames
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML CookieNames where
    toXML CookieNames{..} = nodes "CookieNames"
        [ "Quantity" =@ _cnQuantity
        , "Items"    =@ _cnItems
        ]

data CustomErrorResponse = CustomErrorResponse
    { _cerErrorCachingMinTTL :: Maybe Integer
    , _cerErrorCode          :: Int
    , _cerResponseCode       :: Maybe Text
    , _cerResponsePagePath   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CustomErrorResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerErrorCachingMinTTL' @::@ 'Maybe' 'Integer'
--
-- * 'cerErrorCode' @::@ 'Int'
--
-- * 'cerResponseCode' @::@ 'Maybe' 'Text'
--
-- * 'cerResponsePagePath' @::@ 'Maybe' 'Text'
--
customErrorResponse :: Int -- ^ 'cerErrorCode'
                    -> CustomErrorResponse
customErrorResponse p1 = CustomErrorResponse
    { _cerErrorCode          = p1
    , _cerResponsePagePath   = Nothing
    , _cerResponseCode       = Nothing
    , _cerErrorCachingMinTTL = Nothing
    }

-- | The minimum amount of time you want HTTP error codes to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object has
-- been updated. You can specify a value from 0 to 31,536,000.
cerErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
cerErrorCachingMinTTL =
    lens _cerErrorCachingMinTTL (\s a -> s { _cerErrorCachingMinTTL = a })

-- | The 4xx or 5xx HTTP status code that you want to customize. For a list of
-- HTTP status codes that you can customize, see CloudFront documentation.
cerErrorCode :: Lens' CustomErrorResponse Int
cerErrorCode = lens _cerErrorCode (\s a -> s { _cerErrorCode = a })

-- | The HTTP status code that you want CloudFront to return with the custom error
-- page to the viewer. For a list of HTTP status codes that you can replace, see
-- CloudFront Documentation.
cerResponseCode :: Lens' CustomErrorResponse (Maybe Text)
cerResponseCode = lens _cerResponseCode (\s a -> s { _cerResponseCode = a })

-- | The path of the custom error page (for example, /custom_404.html). The path
-- is relative to the distribution and must begin with a slash (/). If the path
-- includes any non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http://www.ietf.org/rfc/rfc1738.txt), URL encode those characters. Do not
-- URL encode any other characters in the path, or CloudFront will not return
-- the custom error page to the viewer.
cerResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
cerResponsePagePath =
    lens _cerResponsePagePath (\s a -> s { _cerResponsePagePath = a })

instance FromXML CustomErrorResponse where
    parseXML x = CustomErrorResponse
        <$> x .@? "ErrorCachingMinTTL"
        <*> x .@  "ErrorCode"
        <*> x .@? "ResponseCode"
        <*> x .@? "ResponsePagePath"

instance ToXML CustomErrorResponse where
    toXML CustomErrorResponse{..} = nodes "CustomErrorResponse"
        [ "ErrorCode"          =@ _cerErrorCode
        , "ResponsePagePath"   =@ _cerResponsePagePath
        , "ResponseCode"       =@ _cerResponseCode
        , "ErrorCachingMinTTL" =@ _cerErrorCachingMinTTL
        ]

data CacheBehaviors = CacheBehaviors
    { _cbItems    :: List "CacheBehavior" CacheBehavior
    , _cbQuantity :: Int
    } deriving (Eq, Read, Show)

-- | 'CacheBehaviors' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbItems' @::@ ['CacheBehavior']
--
-- * 'cbQuantity' @::@ 'Int'
--
cacheBehaviors :: Int -- ^ 'cbQuantity'
               -> CacheBehaviors
cacheBehaviors p1 = CacheBehaviors
    { _cbQuantity = p1
    , _cbItems    = mempty
    }

-- | Optional: A complex type that contains cache behaviors for this distribution.
-- If Quantity is 0, you can omit Items.
cbItems :: Lens' CacheBehaviors [CacheBehavior]
cbItems = lens _cbItems (\s a -> s { _cbItems = a }) . _List

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors Int
cbQuantity = lens _cbQuantity (\s a -> s { _cbQuantity = a })

instance FromXML CacheBehaviors where
    parseXML x = CacheBehaviors
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML CacheBehaviors where
    toXML CacheBehaviors{..} = nodes "CacheBehaviors"
        [ "Quantity" =@ _cbQuantity
        , "Items"    =@ _cbItems
        ]

data DefaultCacheBehavior = DefaultCacheBehavior
    { _dcbAllowedMethods       :: Maybe AllowedMethods
    , _dcbForwardedValues      :: ForwardedValues
    , _dcbMinTTL               :: Integer
    , _dcbSmoothStreaming      :: Maybe Bool
    , _dcbTargetOriginId       :: Text
    , _dcbTrustedSigners       :: TrustedSigners
    , _dcbViewerProtocolPolicy :: ViewerProtocolPolicy
    } deriving (Eq, Read, Show)

-- | 'DefaultCacheBehavior' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcbAllowedMethods' @::@ 'Maybe' 'AllowedMethods'
--
-- * 'dcbForwardedValues' @::@ 'ForwardedValues'
--
-- * 'dcbMinTTL' @::@ 'Integer'
--
-- * 'dcbSmoothStreaming' @::@ 'Maybe' 'Bool'
--
-- * 'dcbTargetOriginId' @::@ 'Text'
--
-- * 'dcbTrustedSigners' @::@ 'TrustedSigners'
--
-- * 'dcbViewerProtocolPolicy' @::@ 'ViewerProtocolPolicy'
--
defaultCacheBehavior :: Text -- ^ 'dcbTargetOriginId'
                     -> ForwardedValues -- ^ 'dcbForwardedValues'
                     -> TrustedSigners -- ^ 'dcbTrustedSigners'
                     -> ViewerProtocolPolicy -- ^ 'dcbViewerProtocolPolicy'
                     -> Integer -- ^ 'dcbMinTTL'
                     -> DefaultCacheBehavior
defaultCacheBehavior p1 p2 p3 p4 p5 = DefaultCacheBehavior
    { _dcbTargetOriginId       = p1
    , _dcbForwardedValues      = p2
    , _dcbTrustedSigners       = p3
    , _dcbViewerProtocolPolicy = p4
    , _dcbMinTTL               = p5
    , _dcbAllowedMethods       = Nothing
    , _dcbSmoothStreaming      = Nothing
    }

dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods =
    lens _dcbAllowedMethods (\s a -> s { _dcbAllowedMethods = a })

-- | A complex type that specifies how CloudFront handles query strings, cookies
-- and headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior ForwardedValues
dcbForwardedValues =
    lens _dcbForwardedValues (\s a -> s { _dcbForwardedValues = a })

-- | The minimum amount of time that you want objects to stay in CloudFront caches
-- before CloudFront queries your origin to see whether the object has been
-- updated.You can specify a value from 0 to 3,153,600,000 seconds (100 years).
dcbMinTTL :: Lens' DefaultCacheBehavior Integer
dcbMinTTL = lens _dcbMinTTL (\s a -> s { _dcbMinTTL = a })

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming =
    lens _dcbSmoothStreaming (\s a -> s { _dcbSmoothStreaming = a })

-- | The value of ID for the origin that you want CloudFront to route requests to
-- when a request matches the path pattern either for a cache behavior or for
-- the default cache behavior.
dcbTargetOriginId :: Lens' DefaultCacheBehavior Text
dcbTargetOriginId =
    lens _dcbTargetOriginId (\s a -> s { _dcbTargetOriginId = a })

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and specify
-- the applicable values for Quantity and Items. For more information, go to
-- Using a Signed URL to Serve Private Content in the Amazon CloudFront
-- Developer Guide. If you don't want to require signed URLs in requests for
-- objects that match PathPattern, specify false for Enabled and 0 for Quantity.
-- Omit Items. To add, change, or remove one or more trusted signers, change
-- Enabled to true (if it's currently false), change Quantity as applicable, and
-- specify all of the trusted signers that you want to include in the updated
-- distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior TrustedSigners
dcbTrustedSigners =
    lens _dcbTrustedSigners (\s a -> s { _dcbTrustedSigners = a })

-- | Use this element to specify the protocol that users can use to access the
-- files in the origin specified by TargetOriginId when a request matches the
-- path pattern in PathPattern. If you want CloudFront to allow end users to use
-- any available protocol, specify allow-all. If you want CloudFront to require
-- HTTPS, specify https. If you want CloudFront to respond to an HTTP request
-- with an HTTP status code of 301 (Moved Permanently) and the HTTPS URL,
-- specify redirect-to-https. The viewer then resubmits the request using the
-- HTTPS URL.
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior ViewerProtocolPolicy
dcbViewerProtocolPolicy =
    lens _dcbViewerProtocolPolicy (\s a -> s { _dcbViewerProtocolPolicy = a })

instance FromXML DefaultCacheBehavior where
    parseXML x = DefaultCacheBehavior
        <$> x .@? "AllowedMethods"
        <*> x .@  "ForwardedValues"
        <*> x .@  "MinTTL"
        <*> x .@? "SmoothStreaming"
        <*> x .@  "TargetOriginId"
        <*> x .@  "TrustedSigners"
        <*> x .@  "ViewerProtocolPolicy"

instance ToXML DefaultCacheBehavior where
    toXML DefaultCacheBehavior{..} = nodes "DefaultCacheBehavior"
        [ "TargetOriginId"       =@ _dcbTargetOriginId
        , "ForwardedValues"      =@ _dcbForwardedValues
        , "TrustedSigners"       =@ _dcbTrustedSigners
        , "ViewerProtocolPolicy" =@ _dcbViewerProtocolPolicy
        , "MinTTL"               =@ _dcbMinTTL
        , "AllowedMethods"       =@ _dcbAllowedMethods
        , "SmoothStreaming"      =@ _dcbSmoothStreaming
        ]

data InvalidationList = InvalidationList
    { _ilIsTruncated :: Bool
    , _ilItems       :: List "InvalidationSummary" InvalidationSummary
    , _ilMarker      :: Text
    , _ilMaxItems    :: Int
    , _ilNextMarker  :: Maybe Text
    , _ilQuantity    :: Int
    } deriving (Eq, Read, Show)

-- | 'InvalidationList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ilIsTruncated' @::@ 'Bool'
--
-- * 'ilItems' @::@ ['InvalidationSummary']
--
-- * 'ilMarker' @::@ 'Text'
--
-- * 'ilMaxItems' @::@ 'Int'
--
-- * 'ilNextMarker' @::@ 'Maybe' 'Text'
--
-- * 'ilQuantity' @::@ 'Int'
--
invalidationList :: Text -- ^ 'ilMarker'
                 -> Int -- ^ 'ilMaxItems'
                 -> Bool -- ^ 'ilIsTruncated'
                 -> Int -- ^ 'ilQuantity'
                 -> InvalidationList
invalidationList p1 p2 p3 p4 = InvalidationList
    { _ilMarker      = p1
    , _ilMaxItems    = p2
    , _ilIsTruncated = p3
    , _ilQuantity    = p4
    , _ilNextMarker  = Nothing
    , _ilItems       = mempty
    }

-- | A flag that indicates whether more invalidation batch requests remain to be
-- listed. If your results were truncated, you can make a follow-up pagination
-- request using the Marker request parameter to retrieve more invalidation
-- batches in the list.
ilIsTruncated :: Lens' InvalidationList Bool
ilIsTruncated = lens _ilIsTruncated (\s a -> s { _ilIsTruncated = a })

-- | A complex type that contains one InvalidationSummary element for each
-- invalidation batch that was created by the current AWS account.
ilItems :: Lens' InvalidationList [InvalidationSummary]
ilItems = lens _ilItems (\s a -> s { _ilItems = a }) . _List

-- | The value you provided for the Marker request parameter.
ilMarker :: Lens' InvalidationList Text
ilMarker = lens _ilMarker (\s a -> s { _ilMarker = a })

-- | The value you provided for the MaxItems request parameter.
ilMaxItems :: Lens' InvalidationList Int
ilMaxItems = lens _ilMaxItems (\s a -> s { _ilMaxItems = a })

-- | If IsTruncated is true, this element is present and contains the value you
-- can use for the Marker request parameter to continue listing your
-- invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\s a -> s { _ilNextMarker = a })

-- | The number of invalidation batches that were created by the current AWS
-- account.
ilQuantity :: Lens' InvalidationList Int
ilQuantity = lens _ilQuantity (\s a -> s { _ilQuantity = a })

instance FromXML InvalidationList where
    parseXML x = InvalidationList
        <$> x .@  "IsTruncated"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Marker"
        <*> x .@  "MaxItems"
        <*> x .@? "NextMarker"
        <*> x .@  "Quantity"

instance ToXML InvalidationList where
    toXML InvalidationList{..} = nodes "InvalidationList"
        [ "Marker"      =@ _ilMarker
        , "NextMarker"  =@ _ilNextMarker
        , "MaxItems"    =@ _ilMaxItems
        , "IsTruncated" =@ _ilIsTruncated
        , "Quantity"    =@ _ilQuantity
        , "Items"       =@ _ilItems
        ]

data StreamingDistribution = StreamingDistribution
    { _sdActiveTrustedSigners        :: ActiveTrustedSigners
    , _sdDomainName                  :: Text
    , _sdId                          :: Text
    , _sdLastModifiedTime            :: Maybe ISO8601
    , _sdStatus                      :: Text
    , _sdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq, Read, Show)

-- | 'StreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdActiveTrustedSigners' @::@ 'ActiveTrustedSigners'
--
-- * 'sdDomainName' @::@ 'Text'
--
-- * 'sdId' @::@ 'Text'
--
-- * 'sdLastModifiedTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sdStatus' @::@ 'Text'
--
-- * 'sdStreamingDistributionConfig' @::@ 'StreamingDistributionConfig'
--
streamingDistribution :: Text -- ^ 'sdId'
                      -> Text -- ^ 'sdStatus'
                      -> Text -- ^ 'sdDomainName'
                      -> ActiveTrustedSigners -- ^ 'sdActiveTrustedSigners'
                      -> StreamingDistributionConfig -- ^ 'sdStreamingDistributionConfig'
                      -> StreamingDistribution
streamingDistribution p1 p2 p3 p4 p5 = StreamingDistribution
    { _sdId                          = p1
    , _sdStatus                      = p2
    , _sdDomainName                  = p3
    , _sdActiveTrustedSigners        = p4
    , _sdStreamingDistributionConfig = p5
    , _sdLastModifiedTime            = Nothing
    }

-- | CloudFront automatically adds this element to the response only if you've set
-- up the distribution to serve private content with signed URLs. The element
-- lists the key pair IDs that CloudFront is aware of for each trusted signer.
-- The Signer child element lists the AWS account number of the trusted signer
-- (or an empty Self element if the signer is you). The Signer element also
-- includes the IDs of any active key pairs associated with the trusted signer's
-- AWS account. If no KeyPairId element appears for a Signer, that signer can't
-- create working signed URLs.
sdActiveTrustedSigners :: Lens' StreamingDistribution ActiveTrustedSigners
sdActiveTrustedSigners =
    lens _sdActiveTrustedSigners (\s a -> s { _sdActiveTrustedSigners = a })

-- | The domain name corresponding to the streaming distribution. For example:
-- s5c39gqb8ow64r.cloudfront.net.
sdDomainName :: Lens' StreamingDistribution Text
sdDomainName = lens _sdDomainName (\s a -> s { _sdDomainName = a })

-- | The identifier for the streaming distribution. For example: EGTXBD79H29TRA8.
sdId :: Lens' StreamingDistribution Text
sdId = lens _sdId (\s a -> s { _sdId = a })

-- | The date and time the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe UTCTime)
sdLastModifiedTime =
    lens _sdLastModifiedTime (\s a -> s { _sdLastModifiedTime = a })
        . mapping _Time

-- | The current status of the streaming distribution. When the status is
-- Deployed, the distribution's information is fully propagated throughout the
-- Amazon CloudFront system.
sdStatus :: Lens' StreamingDistribution Text
sdStatus = lens _sdStatus (\s a -> s { _sdStatus = a })

-- | The current configuration information for the streaming distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution StreamingDistributionConfig
sdStreamingDistributionConfig =
    lens _sdStreamingDistributionConfig
        (\s a -> s { _sdStreamingDistributionConfig = a })

instance FromXML StreamingDistribution where
    parseXML x = StreamingDistribution
        <$> x .@  "ActiveTrustedSigners"
        <*> x .@  "DomainName"
        <*> x .@  "Id"
        <*> x .@? "LastModifiedTime"
        <*> x .@  "Status"
        <*> x .@  "StreamingDistributionConfig"

instance ToXMLRoot StreamingDistribution where
    toXMLRoot StreamingDistribution{..} = namespaced ns "StreamingDistribution"
        [ "Id"                          =@ _sdId
        , "Status"                      =@ _sdStatus
        , "LastModifiedTime"            =@ _sdLastModifiedTime
        , "DomainName"                  =@ _sdDomainName
        , "ActiveTrustedSigners"        =@ _sdActiveTrustedSigners
        , "StreamingDistributionConfig" =@ _sdStreamingDistributionConfig
        ]

instance ToXML StreamingDistribution

data Paths = Paths
    { _pItems    :: List "Path" Text
    , _pQuantity :: Int
    } deriving (Eq, Ord, Read, Show)

-- | 'Paths' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pItems' @::@ ['Text']
--
-- * 'pQuantity' @::@ 'Int'
--
paths :: Int -- ^ 'pQuantity'
      -> Paths
paths p1 = Paths
    { _pQuantity = p1
    , _pItems    = mempty
    }

-- | A complex type that contains a list of the objects that you want to
-- invalidate.
pItems :: Lens' Paths [Text]
pItems = lens _pItems (\s a -> s { _pItems = a }) . _List

-- | The number of objects that you want to invalidate.
pQuantity :: Lens' Paths Int
pQuantity = lens _pQuantity (\s a -> s { _pQuantity = a })

instance FromXML Paths where
    parseXML x = Paths
        <$> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML Paths where
    toXML Paths{..} = nodes "Paths"
        [ "Quantity" =@ _pQuantity
        , "Items"    =@ _pItems
        ]

data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity
    { _cfoaiCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
    , _cfoaiId                                   :: Text
    , _cfoaiS3CanonicalUserId                    :: Text
    } deriving (Eq, Read, Show)

-- | 'CloudFrontOriginAccessIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaiCloudFrontOriginAccessIdentityConfig' @::@ 'Maybe' 'CloudFrontOriginAccessIdentityConfig'
--
-- * 'cfoaiId' @::@ 'Text'
--
-- * 'cfoaiS3CanonicalUserId' @::@ 'Text'
--
cloudFrontOriginAccessIdentity :: Text -- ^ 'cfoaiId'
                               -> Text -- ^ 'cfoaiS3CanonicalUserId'
                               -> CloudFrontOriginAccessIdentity
cloudFrontOriginAccessIdentity p1 p2 = CloudFrontOriginAccessIdentity
    { _cfoaiId                                   = p1
    , _cfoaiS3CanonicalUserId                    = p2
    , _cfoaiCloudFrontOriginAccessIdentityConfig = Nothing
    }

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig =
    lens _cfoaiCloudFrontOriginAccessIdentityConfig
        (\s a -> s { _cfoaiCloudFrontOriginAccessIdentityConfig = a })

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaiId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiId = lens _cfoaiId (\s a -> s { _cfoaiId = a })

-- | The Amazon S3 canonical user ID for the origin access identity, which you use
-- when giving the origin access identity read permission to an object in Amazon
-- S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiS3CanonicalUserId =
    lens _cfoaiS3CanonicalUserId (\s a -> s { _cfoaiS3CanonicalUserId = a })

instance FromXML CloudFrontOriginAccessIdentity where
    parseXML x = CloudFrontOriginAccessIdentity
        <$> x .@? "CloudFrontOriginAccessIdentityConfig"
        <*> x .@  "Id"
        <*> x .@  "S3CanonicalUserId"

instance ToXMLRoot CloudFrontOriginAccessIdentity where
    toXMLRoot CloudFrontOriginAccessIdentity{..} = namespaced ns "CloudFrontOriginAccessIdentity"
        [ "Id"                                   =@ _cfoaiId
        , "S3CanonicalUserId"                    =@ _cfoaiS3CanonicalUserId
        , "CloudFrontOriginAccessIdentityConfig" =@ _cfoaiCloudFrontOriginAccessIdentityConfig
        ]

instance ToXML CloudFrontOriginAccessIdentity

data ActiveTrustedSigners = ActiveTrustedSigners
    { _atsEnabled  :: Bool
    , _atsItems    :: List "Signer" Signer
    , _atsQuantity :: Int
    } deriving (Eq, Read, Show)

-- | 'ActiveTrustedSigners' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atsEnabled' @::@ 'Bool'
--
-- * 'atsItems' @::@ ['Signer']
--
-- * 'atsQuantity' @::@ 'Int'
--
activeTrustedSigners :: Bool -- ^ 'atsEnabled'
                     -> Int -- ^ 'atsQuantity'
                     -> ActiveTrustedSigners
activeTrustedSigners p1 p2 = ActiveTrustedSigners
    { _atsEnabled  = p1
    , _atsQuantity = p2
    , _atsItems    = mempty
    }

-- | Each active trusted signer.
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\s a -> s { _atsEnabled = a })

-- | A complex type that contains one Signer complex type for each unique trusted
-- signer that is specified in the TrustedSigners complex type, including
-- trusted signers in the default cache behavior and in all of the other cache
-- behaviors.
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\s a -> s { _atsItems = a }) . _List

-- | The number of unique trusted signers included in all cache behaviors. For
-- example, if three cache behaviors all list the same three AWS accounts, the
-- value of Quantity for ActiveTrustedSigners will be 3.
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\s a -> s { _atsQuantity = a })

instance FromXML ActiveTrustedSigners where
    parseXML x = ActiveTrustedSigners
        <$> x .@  "Enabled"
        <*> x .@? "Items" .!@ mempty
        <*> x .@  "Quantity"

instance ToXML ActiveTrustedSigners where
    toXML ActiveTrustedSigners{..} = nodes "ActiveTrustedSigners"
        [ "Enabled"  =@ _atsEnabled
        , "Quantity" =@ _atsQuantity
        , "Items"    =@ _atsItems
        ]

data DistributionSummary = DistributionSummary
    { _dsAliases              :: Aliases
    , _dsCacheBehaviors       :: CacheBehaviors
    , _dsComment              :: Text
    , _dsCustomErrorResponses :: CustomErrorResponses
    , _dsDefaultCacheBehavior :: DefaultCacheBehavior
    , _dsDomainName           :: Text
    , _dsEnabled              :: Bool
    , _dsId                   :: Text
    , _dsLastModifiedTime     :: ISO8601
    , _dsOrigins              :: Origins
    , _dsPriceClass           :: PriceClass
    , _dsRestrictions         :: Restrictions
    , _dsStatus               :: Text
    , _dsViewerCertificate    :: ViewerCertificate
    } deriving (Eq, Read, Show)

-- | 'DistributionSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsAliases' @::@ 'Aliases'
--
-- * 'dsCacheBehaviors' @::@ 'CacheBehaviors'
--
-- * 'dsComment' @::@ 'Text'
--
-- * 'dsCustomErrorResponses' @::@ 'CustomErrorResponses'
--
-- * 'dsDefaultCacheBehavior' @::@ 'DefaultCacheBehavior'
--
-- * 'dsDomainName' @::@ 'Text'
--
-- * 'dsEnabled' @::@ 'Bool'
--
-- * 'dsId' @::@ 'Text'
--
-- * 'dsLastModifiedTime' @::@ 'UTCTime'
--
-- * 'dsOrigins' @::@ 'Origins'
--
-- * 'dsPriceClass' @::@ 'PriceClass'
--
-- * 'dsRestrictions' @::@ 'Restrictions'
--
-- * 'dsStatus' @::@ 'Text'
--
-- * 'dsViewerCertificate' @::@ 'ViewerCertificate'
--
distributionSummary :: Text -- ^ 'dsId'
                    -> Text -- ^ 'dsStatus'
                    -> UTCTime -- ^ 'dsLastModifiedTime'
                    -> Text -- ^ 'dsDomainName'
                    -> Aliases -- ^ 'dsAliases'
                    -> Origins -- ^ 'dsOrigins'
                    -> DefaultCacheBehavior -- ^ 'dsDefaultCacheBehavior'
                    -> CacheBehaviors -- ^ 'dsCacheBehaviors'
                    -> CustomErrorResponses -- ^ 'dsCustomErrorResponses'
                    -> Text -- ^ 'dsComment'
                    -> PriceClass -- ^ 'dsPriceClass'
                    -> Bool -- ^ 'dsEnabled'
                    -> ViewerCertificate -- ^ 'dsViewerCertificate'
                    -> Restrictions -- ^ 'dsRestrictions'
                    -> DistributionSummary
distributionSummary p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 = DistributionSummary
    { _dsId                   = p1
    , _dsStatus               = p2
    , _dsLastModifiedTime     = withIso _Time (const id) p3
    , _dsDomainName           = p4
    , _dsAliases              = p5
    , _dsOrigins              = p6
    , _dsDefaultCacheBehavior = p7
    , _dsCacheBehaviors       = p8
    , _dsCustomErrorResponses = p9
    , _dsComment              = p10
    , _dsPriceClass           = p11
    , _dsEnabled              = p12
    , _dsViewerCertificate    = p13
    , _dsRestrictions         = p14
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary Aliases
dsAliases = lens _dsAliases (\s a -> s { _dsAliases = a })

-- | A complex type that contains zero or more CacheBehavior elements.
dsCacheBehaviors :: Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = lens _dsCacheBehaviors (\s a -> s { _dsCacheBehaviors = a })

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary Text
dsComment = lens _dsComment (\s a -> s { _dsComment = a })

-- | A complex type that contains zero or more CustomErrorResponses elements.
dsCustomErrorResponses :: Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses =
    lens _dsCustomErrorResponses (\s a -> s { _dsCustomErrorResponses = a })

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don't match any of the values of
-- PathPattern in CacheBehavior elements.You must create exactly one default
-- cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior =
    lens _dsDefaultCacheBehavior (\s a -> s { _dsDefaultCacheBehavior = a })

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dsDomainName :: Lens' DistributionSummary Text
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })

-- | Whether the distribution is enabled to accept end user requests for content.
dsEnabled :: Lens' DistributionSummary Bool
dsEnabled = lens _dsEnabled (\s a -> s { _dsEnabled = a })

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dsId :: Lens' DistributionSummary Text
dsId = lens _dsId (\s a -> s { _dsId = a })

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary UTCTime
dsLastModifiedTime =
    lens _dsLastModifiedTime (\s a -> s { _dsLastModifiedTime = a })
        . _Time

-- | A complex type that contains information about origins for this distribution.
dsOrigins :: Lens' DistributionSummary Origins
dsOrigins = lens _dsOrigins (\s a -> s { _dsOrigins = a })

dsPriceClass :: Lens' DistributionSummary PriceClass
dsPriceClass = lens _dsPriceClass (\s a -> s { _dsPriceClass = a })

dsRestrictions :: Lens' DistributionSummary Restrictions
dsRestrictions = lens _dsRestrictions (\s a -> s { _dsRestrictions = a })

-- | This response element indicates the current status of the distribution. When
-- the status is Deployed, the distribution's information is fully propagated
-- throughout the Amazon CloudFront system.
dsStatus :: Lens' DistributionSummary Text
dsStatus = lens _dsStatus (\s a -> s { _dsStatus = a })

dsViewerCertificate :: Lens' DistributionSummary ViewerCertificate
dsViewerCertificate =
    lens _dsViewerCertificate (\s a -> s { _dsViewerCertificate = a })

instance FromXML DistributionSummary where
    parseXML x = DistributionSummary
        <$> x .@  "Aliases"
        <*> x .@  "CacheBehaviors"
        <*> x .@  "Comment"
        <*> x .@  "CustomErrorResponses"
        <*> x .@  "DefaultCacheBehavior"
        <*> x .@  "DomainName"
        <*> x .@  "Enabled"
        <*> x .@  "Id"
        <*> x .@  "LastModifiedTime"
        <*> x .@  "Origins"
        <*> x .@  "PriceClass"
        <*> x .@  "Restrictions"
        <*> x .@  "Status"
        <*> x .@  "ViewerCertificate"

instance ToXML DistributionSummary where
    toXML DistributionSummary{..} = nodes "DistributionSummary"
        [ "Id"                   =@ _dsId
        , "Status"               =@ _dsStatus
        , "LastModifiedTime"     =@ _dsLastModifiedTime
        , "DomainName"           =@ _dsDomainName
        , "Aliases"              =@ _dsAliases
        , "Origins"              =@ _dsOrigins
        , "DefaultCacheBehavior" =@ _dsDefaultCacheBehavior
        , "CacheBehaviors"       =@ _dsCacheBehaviors
        , "CustomErrorResponses" =@ _dsCustomErrorResponses
        , "Comment"              =@ _dsComment
        , "PriceClass"           =@ _dsPriceClass
        , "Enabled"              =@ _dsEnabled
        , "ViewerCertificate"    =@ _dsViewerCertificate
        , "Restrictions"         =@ _dsRestrictions
        ]

data GeoRestrictionType
    = GRTBlacklist -- ^ blacklist
    | GRTNone      -- ^ none
    | GRTWhitelist -- ^ whitelist
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable GeoRestrictionType

instance FromText GeoRestrictionType where
    parser = takeLowerText >>= \case
        "blacklist" -> pure GRTBlacklist
        "none"      -> pure GRTNone
        "whitelist" -> pure GRTWhitelist
        e           -> fail $
            "Failure parsing GeoRestrictionType from " ++ show e

instance ToText GeoRestrictionType where
    toText = \case
        GRTBlacklist -> "blacklist"
        GRTNone      -> "none"
        GRTWhitelist -> "whitelist"

instance ToByteString GeoRestrictionType
instance ToHeader     GeoRestrictionType
instance ToQuery      GeoRestrictionType

instance FromXML GeoRestrictionType where
    parseXML = parseXMLText "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXML = toXMLText

data LoggingConfig = LoggingConfig
    { _lcBucket         :: Text
    , _lcEnabled        :: Bool
    , _lcIncludeCookies :: Bool
    , _lcPrefix         :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'LoggingConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcBucket' @::@ 'Text'
--
-- * 'lcEnabled' @::@ 'Bool'
--
-- * 'lcIncludeCookies' @::@ 'Bool'
--
-- * 'lcPrefix' @::@ 'Text'
--
loggingConfig :: Bool -- ^ 'lcEnabled'
              -> Bool -- ^ 'lcIncludeCookies'
              -> Text -- ^ 'lcBucket'
              -> Text -- ^ 'lcPrefix'
              -> LoggingConfig
loggingConfig p1 p2 p3 p4 = LoggingConfig
    { _lcEnabled        = p1
    , _lcIncludeCookies = p2
    , _lcBucket         = p3
    , _lcPrefix         = p4
    }

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
lcBucket :: Lens' LoggingConfig Text
lcBucket = lens _lcBucket (\s a -> s { _lcBucket = a })

-- | Specifies whether you want CloudFront to save access logs to an Amazon S3
-- bucket. If you do not want to enable logging when you create a distribution
-- or if you want to disable logging for an existing distribution, specify false
-- for Enabled, and specify empty Bucket and Prefix elements. If you specify
-- false for Enabled but you specify values for Bucket, prefix and
-- IncludeCookies, the values are automatically deleted.
lcEnabled :: Lens' LoggingConfig Bool
lcEnabled = lens _lcEnabled (\s a -> s { _lcEnabled = a })

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify true for IncludeCookies. If you choose to include cookies in logs,
-- CloudFront logs all cookies regardless of how you configure the cache
-- behaviors for this distribution. If you do not want to include cookies when
-- you create a distribution or if you want to disable include cookies for an
-- existing distribution, specify false for IncludeCookies.
lcIncludeCookies :: Lens' LoggingConfig Bool
lcIncludeCookies = lens _lcIncludeCookies (\s a -> s { _lcIncludeCookies = a })

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this distribution, for example, myprefix/. If you want to
-- enable logging, but you do not want to specify a prefix, you still must
-- include an empty Prefix element in the Logging element.
lcPrefix :: Lens' LoggingConfig Text
lcPrefix = lens _lcPrefix (\s a -> s { _lcPrefix = a })

instance FromXML LoggingConfig where
    parseXML x = LoggingConfig
        <$> x .@  "Bucket"
        <*> x .@  "Enabled"
        <*> x .@  "IncludeCookies"
        <*> x .@  "Prefix"

instance ToXML LoggingConfig where
    toXML LoggingConfig{..} = nodes "LoggingConfig"
        [ "Enabled"        =@ _lcEnabled
        , "IncludeCookies" =@ _lcIncludeCookies
        , "Bucket"         =@ _lcBucket
        , "Prefix"         =@ _lcPrefix
        ]
