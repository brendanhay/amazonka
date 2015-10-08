{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Product where

import           Network.AWS.CloudFront.Types.Sum
import           Network.AWS.Prelude

-- | A complex type that lists the AWS accounts, if any, that you included in
-- the TrustedSigners complex type for the default cache behavior or for
-- any of the other cache behaviors for this distribution. These are
-- accounts that you want to allow to create signed URLs for private
-- content.
--
-- /See:/ 'activeTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
    { _atsItems    :: !(Maybe [Signer])
    , _atsEnabled  :: !Bool
    , _atsQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ActiveTrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsItems'
--
-- * 'atsEnabled'
--
-- * 'atsQuantity'
activeTrustedSigners
    :: Bool -- ^ 'atsEnabled'
    -> Int -- ^ 'atsQuantity'
    -> ActiveTrustedSigners
activeTrustedSigners pEnabled_ pQuantity_ =
    ActiveTrustedSigners'
    { _atsItems = Nothing
    , _atsEnabled = pEnabled_
    , _atsQuantity = pQuantity_
    }

-- | A complex type that contains one Signer complex type for each unique
-- trusted signer that is specified in the TrustedSigners complex type,
-- including trusted signers in the default cache behavior and in all of
-- the other cache behaviors.
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\ s a -> s{_atsItems = a}) . _Default . _Coerce;

-- | Each active trusted signer.
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\ s a -> s{_atsEnabled = a});

-- | The number of unique trusted signers included in all cache behaviors.
-- For example, if three cache behaviors all list the same three AWS
-- accounts, the value of Quantity for ActiveTrustedSigners will be 3.
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\ s a -> s{_atsQuantity = a});

instance FromXML ActiveTrustedSigners where
        parseXML x
          = ActiveTrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Signer"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- /See:/ 'aliases' smart constructor.
data Aliases = Aliases'
    { _aItems    :: !(Maybe [Text])
    , _aQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Aliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aItems'
--
-- * 'aQuantity'
aliases
    :: Int -- ^ 'aQuantity'
    -> Aliases
aliases pQuantity_ =
    Aliases'
    { _aItems = Nothing
    , _aQuantity = pQuantity_
    }

-- | Optional: A complex type that contains CNAME elements, if any, for this
-- distribution. If Quantity is 0, you can omit Items.
aItems :: Lens' Aliases [Text]
aItems = lens _aItems (\ s a -> s{_aItems = a}) . _Default . _Coerce;

-- | The number of CNAMEs, if any, for this distribution.
aQuantity :: Lens' Aliases Int
aQuantity = lens _aQuantity (\ s a -> s{_aQuantity = a});

instance FromXML Aliases where
        parseXML x
          = Aliases' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CNAME"))
                <*> (x .@ "Quantity")

instance ToXML Aliases where
        toXML Aliases'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "CNAME" <$> _aItems),
               "Quantity" @= _aQuantity]

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are three
-- choices: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards only GET, HEAD and OPTIONS requests. - CloudFront forwards GET,
-- HEAD, OPTIONS, PUT, PATCH, POST, and DELETE requests. If you pick the
-- third choice, you may need to restrict access to your Amazon S3 bucket
-- or to your custom origin so users can\'t perform operations that you
-- don\'t want them to. For example, you may not want users to have
-- permission to delete objects from your origin.
--
-- /See:/ 'allowedMethods' smart constructor.
data AllowedMethods = AllowedMethods'
    { _amCachedMethods :: !(Maybe CachedMethods)
    , _amQuantity      :: !Int
    , _amItems         :: ![Method]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllowedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amCachedMethods'
--
-- * 'amQuantity'
--
-- * 'amItems'
allowedMethods
    :: Int -- ^ 'amQuantity'
    -> AllowedMethods
allowedMethods pQuantity_ =
    AllowedMethods'
    { _amCachedMethods = Nothing
    , _amQuantity = pQuantity_
    , _amItems = mempty
    }

-- | Undocumented member.
amCachedMethods :: Lens' AllowedMethods (Maybe CachedMethods)
amCachedMethods = lens _amCachedMethods (\ s a -> s{_amCachedMethods = a});

-- | The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for GET and HEAD requests), 3 (for GET, HEAD
-- and OPTIONS requests) and 7 (for GET, HEAD, OPTIONS, PUT, PATCH, POST,
-- and DELETE requests).
amQuantity :: Lens' AllowedMethods Int
amQuantity = lens _amQuantity (\ s a -> s{_amQuantity = a});

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to process and forward to your origin.
amItems :: Lens' AllowedMethods [Method]
amItems = lens _amItems (\ s a -> s{_amItems = a}) . _Coerce;

instance FromXML AllowedMethods where
        parseXML x
          = AllowedMethods' <$>
              (x .@? "CachedMethods") <*> (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance ToXML AllowedMethods where
        toXML AllowedMethods'{..}
          = mconcat
              ["CachedMethods" @= _amCachedMethods,
               "Quantity" @= _amQuantity,
               "Items" @= toXMLList "Method" _amItems]

-- | A complex type that describes how CloudFront processes requests. You can
-- create up to 10 cache behaviors.You must create at least as many cache
-- behaviors (including the default cache behavior) as you have origins if
-- you want CloudFront to distribute objects from all of the origins. Each
-- cache behavior specifies the one origin from which you want CloudFront
-- to get objects. If you have two origins and only the default cache
-- behavior, the default cache behavior will cause CloudFront to get
-- objects from one of the origins, but the other origin will never be
-- used. If you don\'t want to specify any cache behaviors, include only an
-- empty CacheBehaviors element. Don\'t include an empty CacheBehavior
-- element, or CloudFront returns a MalformedXML error. To delete all cache
-- behaviors in an existing distribution, update the distribution
-- configuration and include only an empty CacheBehaviors element. To add,
-- change, or remove one or more cache behaviors, update the distribution
-- configuration and specify all of the cache behaviors that you want to
-- include in the updated distribution.
--
-- /See:/ 'cacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
    { _cbAllowedMethods       :: !(Maybe AllowedMethods)
    , _cbMaxTTL               :: !(Maybe Integer)
    , _cbSmoothStreaming      :: !(Maybe Bool)
    , _cbDefaultTTL           :: !(Maybe Integer)
    , _cbPathPattern          :: !Text
    , _cbTargetOriginId       :: !Text
    , _cbForwardedValues      :: !ForwardedValues
    , _cbTrustedSigners       :: !TrustedSigners
    , _cbViewerProtocolPolicy :: !ViewerProtocolPolicy
    , _cbMinTTL               :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbAllowedMethods'
--
-- * 'cbMaxTTL'
--
-- * 'cbSmoothStreaming'
--
-- * 'cbDefaultTTL'
--
-- * 'cbPathPattern'
--
-- * 'cbTargetOriginId'
--
-- * 'cbForwardedValues'
--
-- * 'cbTrustedSigners'
--
-- * 'cbViewerProtocolPolicy'
--
-- * 'cbMinTTL'
cacheBehavior
    :: Text -- ^ 'cbPathPattern'
    -> Text -- ^ 'cbTargetOriginId'
    -> ForwardedValues -- ^ 'cbForwardedValues'
    -> TrustedSigners -- ^ 'cbTrustedSigners'
    -> ViewerProtocolPolicy -- ^ 'cbViewerProtocolPolicy'
    -> Integer -- ^ 'cbMinTTL'
    -> CacheBehavior
cacheBehavior pPathPattern_ pTargetOriginId_ pForwardedValues_ pTrustedSigners_ pViewerProtocolPolicy_ pMinTTL_ =
    CacheBehavior'
    { _cbAllowedMethods = Nothing
    , _cbMaxTTL = Nothing
    , _cbSmoothStreaming = Nothing
    , _cbDefaultTTL = Nothing
    , _cbPathPattern = pPathPattern_
    , _cbTargetOriginId = pTargetOriginId_
    , _cbForwardedValues = pForwardedValues_
    , _cbTrustedSigners = pTrustedSigners_
    , _cbViewerProtocolPolicy = pViewerProtocolPolicy_
    , _cbMinTTL = pMinTTL_
    }

-- | Undocumented member.
cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\ s a -> s{_cbAllowedMethods = a});

-- | The maximum amount of time (in seconds) that an object is in a
-- CloudFront cache before CloudFront forwards another request to your
-- origin to determine whether the object has been updated. The value that
-- you specify applies only when your origin adds HTTP headers such as
-- Cache-Control max-age, Cache-Control s-maxage, and Expires to objects.
-- You can specify a value from 0 to 3,153,600,000 seconds (100 years).
cbMaxTTL :: Lens' CacheBehavior (Maybe Integer)
cbMaxTTL = lens _cbMaxTTL (\ s a -> s{_cbMaxTTL = a});

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming = lens _cbSmoothStreaming (\ s a -> s{_cbSmoothStreaming = a});

-- | If you don\'t configure your origin to add a Cache-Control max-age
-- directive or an Expires header, DefaultTTL is the default amount of time
-- (in seconds) that an object is in a CloudFront cache before CloudFront
-- forwards another request to your origin to determine whether the object
-- has been updated. The value that you specify applies only when your
-- origin does not add HTTP headers such as Cache-Control max-age,
-- Cache-Control s-maxage, and Expires to objects. You can specify a value
-- from 0 to 3,153,600,000 seconds (100 years).
cbDefaultTTL :: Lens' CacheBehavior (Maybe Integer)
cbDefaultTTL = lens _cbDefaultTTL (\ s a -> s{_cbDefaultTTL = a});

-- | The pattern (for example, images\/*.jpg) that specifies which requests
-- you want this cache behavior to apply to. When CloudFront receives an
-- end-user request, the requested path is compared with path patterns in
-- the order in which cache behaviors are listed in the distribution. The
-- path pattern for the default cache behavior is * and cannot be changed.
-- If the request for an object does not match the path pattern for any
-- cache behaviors, CloudFront applies the behavior in the default cache
-- behavior.
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\ s a -> s{_cbPathPattern = a});

-- | The value of ID for the origin that you want CloudFront to route
-- requests to when a request matches the path pattern either for a cache
-- behavior or for the default cache behavior.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\ s a -> s{_cbTargetOriginId = a});

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
cbForwardedValues :: Lens' CacheBehavior ForwardedValues
cbForwardedValues = lens _cbForwardedValues (\ s a -> s{_cbForwardedValues = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
cbTrustedSigners :: Lens' CacheBehavior TrustedSigners
cbTrustedSigners = lens _cbTrustedSigners (\ s a -> s{_cbTrustedSigners = a});

-- | Use this element to specify the protocol that users can use to access
-- the files in the origin specified by TargetOriginId when a request
-- matches the path pattern in PathPattern. If you want CloudFront to allow
-- end users to use any available protocol, specify allow-all. If you want
-- CloudFront to require HTTPS, specify https. If you want CloudFront to
-- respond to an HTTP request with an HTTP status code of 301 (Moved
-- Permanently) and the HTTPS URL, specify redirect-to-https. The viewer
-- then resubmits the request using the HTTPS URL.
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = lens _cbViewerProtocolPolicy (\ s a -> s{_cbViewerProtocolPolicy = a});

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object
-- has been updated.You can specify a value from 0 to 3,153,600,000 seconds
-- (100 years).
cbMinTTL :: Lens' CacheBehavior Integer
cbMinTTL = lens _cbMinTTL (\ s a -> s{_cbMinTTL = a});

instance FromXML CacheBehavior where
        parseXML x
          = CacheBehavior' <$>
              (x .@? "AllowedMethods") <*> (x .@? "MaxTTL") <*>
                (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@ "PathPattern")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance ToXML CacheBehavior where
        toXML CacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _cbAllowedMethods,
               "MaxTTL" @= _cbMaxTTL,
               "SmoothStreaming" @= _cbSmoothStreaming,
               "DefaultTTL" @= _cbDefaultTTL,
               "PathPattern" @= _cbPathPattern,
               "TargetOriginId" @= _cbTargetOriginId,
               "ForwardedValues" @= _cbForwardedValues,
               "TrustedSigners" @= _cbTrustedSigners,
               "ViewerProtocolPolicy" @= _cbViewerProtocolPolicy,
               "MinTTL" @= _cbMinTTL]

-- | A complex type that contains zero or more CacheBehavior elements.
--
-- /See:/ 'cacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
    { _cbItems    :: !(Maybe [CacheBehavior])
    , _cbQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbItems'
--
-- * 'cbQuantity'
cacheBehaviors
    :: Int -- ^ 'cbQuantity'
    -> CacheBehaviors
cacheBehaviors pQuantity_ =
    CacheBehaviors'
    { _cbItems = Nothing
    , _cbQuantity = pQuantity_
    }

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If Quantity is 0, you can omit Items.
cbItems :: Lens' CacheBehaviors [CacheBehavior]
cbItems = lens _cbItems (\ s a -> s{_cbItems = a}) . _Default . _Coerce;

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors Int
cbQuantity = lens _cbQuantity (\ s a -> s{_cbQuantity = a});

instance FromXML CacheBehaviors where
        parseXML x
          = CacheBehaviors' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CacheBehavior"))
                <*> (x .@ "Quantity")

instance ToXML CacheBehaviors where
        toXML CacheBehaviors'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "CacheBehavior" <$> _cbItems),
               "Quantity" @= _cbQuantity]

-- | A complex type that controls whether CloudFront caches the response to
-- requests using the specified HTTP methods. There are two choices: -
-- CloudFront caches responses to GET and HEAD requests. - CloudFront
-- caches responses to GET, HEAD, and OPTIONS requests. If you pick the
-- second choice for your S3 Origin, you may need to forward
-- Access-Control-Request-Method, Access-Control-Request-Headers and Origin
-- headers for the responses to be cached correctly.
--
-- /See:/ 'cachedMethods' smart constructor.
data CachedMethods = CachedMethods'
    { _cmQuantity :: !Int
    , _cmItems    :: ![Method]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CachedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmQuantity'
--
-- * 'cmItems'
cachedMethods
    :: Int -- ^ 'cmQuantity'
    -> CachedMethods
cachedMethods pQuantity_ =
    CachedMethods'
    { _cmQuantity = pQuantity_
    , _cmItems = mempty
    }

-- | The number of HTTP methods for which you want CloudFront to cache
-- responses. Valid values are 2 (for caching responses to GET and HEAD
-- requests) and 3 (for caching responses to GET, HEAD, and OPTIONS
-- requests).
cmQuantity :: Lens' CachedMethods Int
cmQuantity = lens _cmQuantity (\ s a -> s{_cmQuantity = a});

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to cache responses to.
cmItems :: Lens' CachedMethods [Method]
cmItems = lens _cmItems (\ s a -> s{_cmItems = a}) . _Coerce;

instance FromXML CachedMethods where
        parseXML x
          = CachedMethods' <$>
              (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance ToXML CachedMethods where
        toXML CachedMethods'{..}
          = mconcat
              ["Quantity" @= _cmQuantity,
               "Items" @= toXMLList "Method" _cmItems]

-- | CloudFront origin access identity.
--
-- /See:/ 'cloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
    { _cfoaiCloudFrontOriginAccessIdentityConfig :: !(Maybe CloudFrontOriginAccessIdentityConfig)
    , _cfoaiId                                   :: !Text
    , _cfoaiS3CanonicalUserId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaiCloudFrontOriginAccessIdentityConfig'
--
-- * 'cfoaiId'
--
-- * 'cfoaiS3CanonicalUserId'
cloudFrontOriginAccessIdentity
    :: Text -- ^ 'cfoaiId'
    -> Text -- ^ 'cfoaiS3CanonicalUserId'
    -> CloudFrontOriginAccessIdentity
cloudFrontOriginAccessIdentity pId_ pS3CanonicalUserId_ =
    CloudFrontOriginAccessIdentity'
    { _cfoaiCloudFrontOriginAccessIdentityConfig = Nothing
    , _cfoaiId = pId_
    , _cfoaiS3CanonicalUserId = pS3CanonicalUserId_
    }

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = lens _cfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_cfoaiCloudFrontOriginAccessIdentityConfig = a});

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaiId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiId = lens _cfoaiId (\ s a -> s{_cfoaiId = a});

-- | The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiS3CanonicalUserId = lens _cfoaiS3CanonicalUserId (\ s a -> s{_cfoaiS3CanonicalUserId = a});

instance FromXML CloudFrontOriginAccessIdentity where
        parseXML x
          = CloudFrontOriginAccessIdentity' <$>
              (x .@? "CloudFrontOriginAccessIdentityConfig") <*>
                (x .@ "Id")
                <*> (x .@ "S3CanonicalUserId")

-- | Origin access identity configuration.
--
-- /See:/ 'cloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
    { _cfoaicCallerReference :: !Text
    , _cfoaicComment         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaicCallerReference'
--
-- * 'cfoaicComment'
cloudFrontOriginAccessIdentityConfig
    :: Text -- ^ 'cfoaicCallerReference'
    -> Text -- ^ 'cfoaicComment'
    -> CloudFrontOriginAccessIdentityConfig
cloudFrontOriginAccessIdentityConfig pCallerReference_ pComment_ =
    CloudFrontOriginAccessIdentityConfig'
    { _cfoaicCallerReference = pCallerReference_
    , _cfoaicComment = pComment_
    }

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the
-- CloudFrontOriginAccessIdentityConfig object), a new origin access
-- identity is created. If the CallerReference is a value you already sent
-- in a previous request to create an identity, and the content of the
-- CloudFrontOriginAccessIdentityConfig is identical to the original
-- request (ignoring white space), the response includes the same
-- information returned to the original request. If the CallerReference is
-- a value you already sent in a previous request to create an identity but
-- the content of the CloudFrontOriginAccessIdentityConfig is different
-- from the original request, CloudFront returns a
-- CloudFrontOriginAccessIdentityAlreadyExists error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicCallerReference = lens _cfoaicCallerReference (\ s a -> s{_cfoaicCallerReference = a});

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicComment = lens _cfoaicComment (\ s a -> s{_cfoaicComment = a});

instance FromXML CloudFrontOriginAccessIdentityConfig
         where
        parseXML x
          = CloudFrontOriginAccessIdentityConfig' <$>
              (x .@ "CallerReference") <*> (x .@ "Comment")

instance ToXML CloudFrontOriginAccessIdentityConfig
         where
        toXML CloudFrontOriginAccessIdentityConfig'{..}
          = mconcat
              ["CallerReference" @= _cfoaicCallerReference,
               "Comment" @= _cfoaicComment]

-- | The CloudFrontOriginAccessIdentityList type.
--
-- /See:/ 'cloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
    { _cfoailItems       :: !(Maybe [CloudFrontOriginAccessIdentitySummary])
    , _cfoailNextMarker  :: !(Maybe Text)
    , _cfoailMarker      :: !Text
    , _cfoailMaxItems    :: !Int
    , _cfoailIsTruncated :: !Bool
    , _cfoailQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentityList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoailItems'
--
-- * 'cfoailNextMarker'
--
-- * 'cfoailMarker'
--
-- * 'cfoailMaxItems'
--
-- * 'cfoailIsTruncated'
--
-- * 'cfoailQuantity'
cloudFrontOriginAccessIdentityList
    :: Text -- ^ 'cfoailMarker'
    -> Int -- ^ 'cfoailMaxItems'
    -> Bool -- ^ 'cfoailIsTruncated'
    -> Int -- ^ 'cfoailQuantity'
    -> CloudFrontOriginAccessIdentityList
cloudFrontOriginAccessIdentityList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
    CloudFrontOriginAccessIdentityList'
    { _cfoailItems = Nothing
    , _cfoailNextMarker = Nothing
    , _cfoailMarker = pMarker_
    , _cfoailMaxItems = pMaxItems_
    , _cfoailIsTruncated = pIsTruncated_
    , _cfoailQuantity = pQuantity_
    }

-- | A complex type that contains one CloudFrontOriginAccessIdentitySummary
-- element for each origin access identity that was created by the current
-- AWS account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList [CloudFrontOriginAccessIdentitySummary]
cfoailItems = lens _cfoailItems (\ s a -> s{_cfoailItems = a}) . _Default . _Coerce;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- origin access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\ s a -> s{_cfoailNextMarker = a});

-- | The value you provided for the Marker request parameter.
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList Text
cfoailMarker = lens _cfoailMarker (\ s a -> s{_cfoailMarker = a});

-- | The value you provided for the MaxItems request parameter.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailMaxItems = lens _cfoailMaxItems (\ s a -> s{_cfoailMaxItems = a});

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- items in the list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList Bool
cfoailIsTruncated = lens _cfoailIsTruncated (\ s a -> s{_cfoailIsTruncated = a});

-- | The number of CloudFront origin access identities that were created by
-- the current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailQuantity = lens _cfoailQuantity (\ s a -> s{_cfoailQuantity = a});

instance FromXML CloudFrontOriginAccessIdentityList
         where
        parseXML x
          = CloudFrontOriginAccessIdentityList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may
                   (parseXMLList
                      "CloudFrontOriginAccessIdentitySummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | Summary of the information about a CloudFront origin access identity.
--
-- /See:/ 'cloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
    { _cfoaisId                :: !Text
    , _cfoaisS3CanonicalUserId :: !Text
    , _cfoaisComment           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentitySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaisId'
--
-- * 'cfoaisS3CanonicalUserId'
--
-- * 'cfoaisComment'
cloudFrontOriginAccessIdentitySummary
    :: Text -- ^ 'cfoaisId'
    -> Text -- ^ 'cfoaisS3CanonicalUserId'
    -> Text -- ^ 'cfoaisComment'
    -> CloudFrontOriginAccessIdentitySummary
cloudFrontOriginAccessIdentitySummary pId_ pS3CanonicalUserId_ pComment_ =
    CloudFrontOriginAccessIdentitySummary'
    { _cfoaisId = pId_
    , _cfoaisS3CanonicalUserId = pS3CanonicalUserId_
    , _cfoaisComment = pComment_
    }

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisId = lens _cfoaisId (\ s a -> s{_cfoaisId = a});

-- | The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisS3CanonicalUserId = lens _cfoaisS3CanonicalUserId (\ s a -> s{_cfoaisS3CanonicalUserId = a});

-- | The comment for this origin access identity, as originally specified
-- when created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisComment = lens _cfoaisComment (\ s a -> s{_cfoaisComment = a});

instance FromXML
         CloudFrontOriginAccessIdentitySummary where
        parseXML x
          = CloudFrontOriginAccessIdentitySummary' <$>
              (x .@ "Id") <*> (x .@ "S3CanonicalUserId") <*>
                (x .@ "Comment")

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
--
-- /See:/ 'cookieNames' smart constructor.
data CookieNames = CookieNames'
    { _cnItems    :: !(Maybe [Text])
    , _cnQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CookieNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnItems'
--
-- * 'cnQuantity'
cookieNames
    :: Int -- ^ 'cnQuantity'
    -> CookieNames
cookieNames pQuantity_ =
    CookieNames'
    { _cnItems = Nothing
    , _cnQuantity = pQuantity_
    }

-- | Optional: A complex type that contains whitelisted cookies for this
-- cache behavior. If Quantity is 0, you can omit Items.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\ s a -> s{_cnItems = a}) . _Default . _Coerce;

-- | The number of whitelisted cookies for this cache behavior.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\ s a -> s{_cnQuantity = a});

instance FromXML CookieNames where
        parseXML x
          = CookieNames' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance ToXML CookieNames where
        toXML CookieNames'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _cnItems),
               "Quantity" @= _cnQuantity]

-- | A complex type that specifies the cookie preferences associated with
-- this cache behavior.
--
-- /See:/ 'cookiePreference' smart constructor.
data CookiePreference = CookiePreference'
    { _cpWhitelistedNames :: !(Maybe CookieNames)
    , _cpForward          :: !ItemSelection
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CookiePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpWhitelistedNames'
--
-- * 'cpForward'
cookiePreference
    :: ItemSelection -- ^ 'cpForward'
    -> CookiePreference
cookiePreference pForward_ =
    CookiePreference'
    { _cpWhitelistedNames = Nothing
    , _cpForward = pForward_
    }

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames = lens _cpWhitelistedNames (\ s a -> s{_cpWhitelistedNames = a});

-- | Use this element to specify whether you want CloudFront to forward
-- cookies to the origin that is associated with this cache behavior. You
-- can specify all, none or whitelist. If you choose All, CloudFront
-- forwards all cookies regardless of how many your application uses.
cpForward :: Lens' CookiePreference ItemSelection
cpForward = lens _cpForward (\ s a -> s{_cpForward = a});

instance FromXML CookiePreference where
        parseXML x
          = CookiePreference' <$>
              (x .@? "WhitelistedNames") <*> (x .@ "Forward")

instance ToXML CookiePreference where
        toXML CookiePreference'{..}
          = mconcat
              ["WhitelistedNames" @= _cpWhitelistedNames,
               "Forward" @= _cpForward]

-- | A complex type that describes how you\'d prefer CloudFront to respond to
-- requests that result in either a 4xx or 5xx response. You can control
-- whether a custom error page should be displayed, what the desired
-- response code should be for this error page and how long should the
-- error response be cached by CloudFront. If you don\'t want to specify
-- any custom error responses, include only an empty CustomErrorResponses
-- element. To delete all custom error responses in an existing
-- distribution, update the distribution configuration and include only an
-- empty CustomErrorResponses element. To add, change, or remove one or
-- more custom error responses, update the distribution configuration and
-- specify all of the custom error responses that you want to include in
-- the updated distribution.
--
-- /See:/ 'customErrorResponse' smart constructor.
data CustomErrorResponse = CustomErrorResponse'
    { _ceResponsePagePath   :: !(Maybe Text)
    , _ceResponseCode       :: !(Maybe Text)
    , _ceErrorCachingMinTTL :: !(Maybe Integer)
    , _ceErrorCode          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomErrorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceResponsePagePath'
--
-- * 'ceResponseCode'
--
-- * 'ceErrorCachingMinTTL'
--
-- * 'ceErrorCode'
customErrorResponse
    :: Int -- ^ 'ceErrorCode'
    -> CustomErrorResponse
customErrorResponse pErrorCode_ =
    CustomErrorResponse'
    { _ceResponsePagePath = Nothing
    , _ceResponseCode = Nothing
    , _ceErrorCachingMinTTL = Nothing
    , _ceErrorCode = pErrorCode_
    }

-- | The path of the custom error page (for example, \/custom_404.html). The
-- path is relative to the distribution and must begin with a slash (\/).
-- If the path includes any non-ASCII characters or unsafe characters as
-- defined in RFC 1783 (http:\/\/www.ietf.org\/rfc\/rfc1738.txt), URL
-- encode those characters. Do not URL encode any other characters in the
-- path, or CloudFront will not return the custom error page to the viewer.
ceResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
ceResponsePagePath = lens _ceResponsePagePath (\ s a -> s{_ceResponsePagePath = a});

-- | The HTTP status code that you want CloudFront to return with the custom
-- error page to the viewer. For a list of HTTP status codes that you can
-- replace, see CloudFront Documentation.
ceResponseCode :: Lens' CustomErrorResponse (Maybe Text)
ceResponseCode = lens _ceResponseCode (\ s a -> s{_ceResponseCode = a});

-- | The minimum amount of time you want HTTP error codes to stay in
-- CloudFront caches before CloudFront queries your origin to see whether
-- the object has been updated. You can specify a value from 0 to
-- 31,536,000.
ceErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
ceErrorCachingMinTTL = lens _ceErrorCachingMinTTL (\ s a -> s{_ceErrorCachingMinTTL = a});

-- | The 4xx or 5xx HTTP status code that you want to customize. For a list
-- of HTTP status codes that you can customize, see CloudFront
-- documentation.
ceErrorCode :: Lens' CustomErrorResponse Int
ceErrorCode = lens _ceErrorCode (\ s a -> s{_ceErrorCode = a});

instance FromXML CustomErrorResponse where
        parseXML x
          = CustomErrorResponse' <$>
              (x .@? "ResponsePagePath") <*> (x .@? "ResponseCode")
                <*> (x .@? "ErrorCachingMinTTL")
                <*> (x .@ "ErrorCode")

instance ToXML CustomErrorResponse where
        toXML CustomErrorResponse'{..}
          = mconcat
              ["ResponsePagePath" @= _ceResponsePagePath,
               "ResponseCode" @= _ceResponseCode,
               "ErrorCachingMinTTL" @= _ceErrorCachingMinTTL,
               "ErrorCode" @= _ceErrorCode]

-- | A complex type that contains zero or more CustomErrorResponse elements.
--
-- /See:/ 'customErrorResponses' smart constructor.
data CustomErrorResponses = CustomErrorResponses'
    { _cerItems    :: !(Maybe [CustomErrorResponse])
    , _cerQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomErrorResponses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerItems'
--
-- * 'cerQuantity'
customErrorResponses
    :: Int -- ^ 'cerQuantity'
    -> CustomErrorResponses
customErrorResponses pQuantity_ =
    CustomErrorResponses'
    { _cerItems = Nothing
    , _cerQuantity = pQuantity_
    }

-- | Optional: A complex type that contains custom error responses for this
-- distribution. If Quantity is 0, you can omit Items.
cerItems :: Lens' CustomErrorResponses [CustomErrorResponse]
cerItems = lens _cerItems (\ s a -> s{_cerItems = a}) . _Default . _Coerce;

-- | The number of custom error responses for this distribution.
cerQuantity :: Lens' CustomErrorResponses Int
cerQuantity = lens _cerQuantity (\ s a -> s{_cerQuantity = a});

instance FromXML CustomErrorResponses where
        parseXML x
          = CustomErrorResponses' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CustomErrorResponse"))
                <*> (x .@ "Quantity")

instance ToXML CustomErrorResponses where
        toXML CustomErrorResponses'{..}
          = mconcat
              ["Items" @=
                 toXML
                   (toXMLList "CustomErrorResponse" <$> _cerItems),
               "Quantity" @= _cerQuantity]

-- | A customer origin.
--
-- /See:/ 'customOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
    { _cocHTTPPort             :: !Int
    , _cocHTTPSPort            :: !Int
    , _cocOriginProtocolPolicy :: !OriginProtocolPolicy
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CustomOriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cocHTTPPort'
--
-- * 'cocHTTPSPort'
--
-- * 'cocOriginProtocolPolicy'
customOriginConfig
    :: Int -- ^ 'cocHTTPPort'
    -> Int -- ^ 'cocHTTPSPort'
    -> OriginProtocolPolicy -- ^ 'cocOriginProtocolPolicy'
    -> CustomOriginConfig
customOriginConfig pHTTPPort_ pHTTPSPort_ pOriginProtocolPolicy_ =
    CustomOriginConfig'
    { _cocHTTPPort = pHTTPPort_
    , _cocHTTPSPort = pHTTPSPort_
    , _cocOriginProtocolPolicy = pOriginProtocolPolicy_
    }

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\ s a -> s{_cocHTTPPort = a});

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\ s a -> s{_cocHTTPSPort = a});

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\ s a -> s{_cocOriginProtocolPolicy = a});

instance FromXML CustomOriginConfig where
        parseXML x
          = CustomOriginConfig' <$>
              (x .@ "HTTPPort") <*> (x .@ "HTTPSPort") <*>
                (x .@ "OriginProtocolPolicy")

instance ToXML CustomOriginConfig where
        toXML CustomOriginConfig'{..}
          = mconcat
              ["HTTPPort" @= _cocHTTPPort,
               "HTTPSPort" @= _cocHTTPSPort,
               "OriginProtocolPolicy" @= _cocOriginProtocolPolicy]

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
--
-- /See:/ 'defaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
    { _dcbAllowedMethods       :: !(Maybe AllowedMethods)
    , _dcbMaxTTL               :: !(Maybe Integer)
    , _dcbSmoothStreaming      :: !(Maybe Bool)
    , _dcbDefaultTTL           :: !(Maybe Integer)
    , _dcbTargetOriginId       :: !Text
    , _dcbForwardedValues      :: !ForwardedValues
    , _dcbTrustedSigners       :: !TrustedSigners
    , _dcbViewerProtocolPolicy :: !ViewerProtocolPolicy
    , _dcbMinTTL               :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DefaultCacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbAllowedMethods'
--
-- * 'dcbMaxTTL'
--
-- * 'dcbSmoothStreaming'
--
-- * 'dcbDefaultTTL'
--
-- * 'dcbTargetOriginId'
--
-- * 'dcbForwardedValues'
--
-- * 'dcbTrustedSigners'
--
-- * 'dcbViewerProtocolPolicy'
--
-- * 'dcbMinTTL'
defaultCacheBehavior
    :: Text -- ^ 'dcbTargetOriginId'
    -> ForwardedValues -- ^ 'dcbForwardedValues'
    -> TrustedSigners -- ^ 'dcbTrustedSigners'
    -> ViewerProtocolPolicy -- ^ 'dcbViewerProtocolPolicy'
    -> Integer -- ^ 'dcbMinTTL'
    -> DefaultCacheBehavior
defaultCacheBehavior pTargetOriginId_ pForwardedValues_ pTrustedSigners_ pViewerProtocolPolicy_ pMinTTL_ =
    DefaultCacheBehavior'
    { _dcbAllowedMethods = Nothing
    , _dcbMaxTTL = Nothing
    , _dcbSmoothStreaming = Nothing
    , _dcbDefaultTTL = Nothing
    , _dcbTargetOriginId = pTargetOriginId_
    , _dcbForwardedValues = pForwardedValues_
    , _dcbTrustedSigners = pTrustedSigners_
    , _dcbViewerProtocolPolicy = pViewerProtocolPolicy_
    , _dcbMinTTL = pMinTTL_
    }

-- | Undocumented member.
dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods = lens _dcbAllowedMethods (\ s a -> s{_dcbAllowedMethods = a});

-- | The maximum amount of time (in seconds) that an object is in a
-- CloudFront cache before CloudFront forwards another request to your
-- origin to determine whether the object has been updated. The value that
-- you specify applies only when your origin adds HTTP headers such as
-- Cache-Control max-age, Cache-Control s-maxage, and Expires to objects.
-- You can specify a value from 0 to 3,153,600,000 seconds (100 years).
dcbMaxTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbMaxTTL = lens _dcbMaxTTL (\ s a -> s{_dcbMaxTTL = a});

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming = lens _dcbSmoothStreaming (\ s a -> s{_dcbSmoothStreaming = a});

-- | If you don\'t configure your origin to add a Cache-Control max-age
-- directive or an Expires header, DefaultTTL is the default amount of time
-- (in seconds) that an object is in a CloudFront cache before CloudFront
-- forwards another request to your origin to determine whether the object
-- has been updated. The value that you specify applies only when your
-- origin does not add HTTP headers such as Cache-Control max-age,
-- Cache-Control s-maxage, and Expires to objects. You can specify a value
-- from 0 to 3,153,600,000 seconds (100 years).
dcbDefaultTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbDefaultTTL = lens _dcbDefaultTTL (\ s a -> s{_dcbDefaultTTL = a});

-- | The value of ID for the origin that you want CloudFront to route
-- requests to when a request matches the path pattern either for a cache
-- behavior or for the default cache behavior.
dcbTargetOriginId :: Lens' DefaultCacheBehavior Text
dcbTargetOriginId = lens _dcbTargetOriginId (\ s a -> s{_dcbTargetOriginId = a});

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior ForwardedValues
dcbForwardedValues = lens _dcbForwardedValues (\ s a -> s{_dcbForwardedValues = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior TrustedSigners
dcbTrustedSigners = lens _dcbTrustedSigners (\ s a -> s{_dcbTrustedSigners = a});

-- | Use this element to specify the protocol that users can use to access
-- the files in the origin specified by TargetOriginId when a request
-- matches the path pattern in PathPattern. If you want CloudFront to allow
-- end users to use any available protocol, specify allow-all. If you want
-- CloudFront to require HTTPS, specify https. If you want CloudFront to
-- respond to an HTTP request with an HTTP status code of 301 (Moved
-- Permanently) and the HTTPS URL, specify redirect-to-https. The viewer
-- then resubmits the request using the HTTPS URL.
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior ViewerProtocolPolicy
dcbViewerProtocolPolicy = lens _dcbViewerProtocolPolicy (\ s a -> s{_dcbViewerProtocolPolicy = a});

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object
-- has been updated.You can specify a value from 0 to 3,153,600,000 seconds
-- (100 years).
dcbMinTTL :: Lens' DefaultCacheBehavior Integer
dcbMinTTL = lens _dcbMinTTL (\ s a -> s{_dcbMinTTL = a});

instance FromXML DefaultCacheBehavior where
        parseXML x
          = DefaultCacheBehavior' <$>
              (x .@? "AllowedMethods") <*> (x .@? "MaxTTL") <*>
                (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance ToXML DefaultCacheBehavior where
        toXML DefaultCacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _dcbAllowedMethods,
               "MaxTTL" @= _dcbMaxTTL,
               "SmoothStreaming" @= _dcbSmoothStreaming,
               "DefaultTTL" @= _dcbDefaultTTL,
               "TargetOriginId" @= _dcbTargetOriginId,
               "ForwardedValues" @= _dcbForwardedValues,
               "TrustedSigners" @= _dcbTrustedSigners,
               "ViewerProtocolPolicy" @= _dcbViewerProtocolPolicy,
               "MinTTL" @= _dcbMinTTL]

-- | A distribution.
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'
    { _dId                            :: !Text
    , _dStatus                        :: !Text
    , _dLastModifiedTime              :: !ISO8601
    , _dInProgressInvalidationBatches :: !Int
    , _dDomainName                    :: !Text
    , _dActiveTrustedSigners          :: !ActiveTrustedSigners
    , _dDistributionConfig            :: !DistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId'
--
-- * 'dStatus'
--
-- * 'dLastModifiedTime'
--
-- * 'dInProgressInvalidationBatches'
--
-- * 'dDomainName'
--
-- * 'dActiveTrustedSigners'
--
-- * 'dDistributionConfig'
distribution
    :: Text -- ^ 'dId'
    -> Text -- ^ 'dStatus'
    -> UTCTime -- ^ 'dLastModifiedTime'
    -> Int -- ^ 'dInProgressInvalidationBatches'
    -> Text -- ^ 'dDomainName'
    -> ActiveTrustedSigners -- ^ 'dActiveTrustedSigners'
    -> DistributionConfig -- ^ 'dDistributionConfig'
    -> Distribution
distribution pId_ pStatus_ pLastModifiedTime_ pInProgressInvalidationBatches_ pDomainName_ pActiveTrustedSigners_ pDistributionConfig_ =
    Distribution'
    { _dId = pId_
    , _dStatus = pStatus_
    , _dLastModifiedTime = _Time # pLastModifiedTime_
    , _dInProgressInvalidationBatches = pInProgressInvalidationBatches_
    , _dDomainName = pDomainName_
    , _dActiveTrustedSigners = pActiveTrustedSigners_
    , _dDistributionConfig = pDistributionConfig_
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dId :: Lens' Distribution Text
dId = lens _dId (\ s a -> s{_dId = a});

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution\'s information is fully
-- propagated throughout the Amazon CloudFront system.
dStatus :: Lens' Distribution Text
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | The date and time the distribution was last modified.
dLastModifiedTime :: Lens' Distribution UTCTime
dLastModifiedTime = lens _dLastModifiedTime (\ s a -> s{_dLastModifiedTime = a}) . _Time;

-- | The number of invalidation batches currently in progress.
dInProgressInvalidationBatches :: Lens' Distribution Int
dInProgressInvalidationBatches = lens _dInProgressInvalidationBatches (\ s a -> s{_dInProgressInvalidationBatches = a});

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dDomainName :: Lens' Distribution Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a});

-- | CloudFront automatically adds this element to the response only if
-- you\'ve set up the distribution to serve private content with signed
-- URLs. The element lists the key pair IDs that CloudFront is aware of for
-- each trusted signer. The Signer child element lists the AWS account
-- number of the trusted signer (or an empty Self element if the signer is
-- you). The Signer element also includes the IDs of any active key pairs
-- associated with the trusted signer\'s AWS account. If no KeyPairId
-- element appears for a Signer, that signer can\'t create working signed
-- URLs.
dActiveTrustedSigners :: Lens' Distribution ActiveTrustedSigners
dActiveTrustedSigners = lens _dActiveTrustedSigners (\ s a -> s{_dActiveTrustedSigners = a});

-- | The current configuration information for the distribution.
dDistributionConfig :: Lens' Distribution DistributionConfig
dDistributionConfig = lens _dDistributionConfig (\ s a -> s{_dDistributionConfig = a});

instance FromXML Distribution where
        parseXML x
          = Distribution' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "InProgressInvalidationBatches")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "DistributionConfig")

-- | A distribution Configuration.
--
-- /See:/ 'distributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
    { _dcAliases              :: !(Maybe Aliases)
    , _dcDefaultRootObject    :: !(Maybe Text)
    , _dcPriceClass           :: !(Maybe PriceClass)
    , _dcCustomErrorResponses :: !(Maybe CustomErrorResponses)
    , _dcWebACLId             :: !(Maybe Text)
    , _dcViewerCertificate    :: !(Maybe ViewerCertificate)
    , _dcRestrictions         :: !(Maybe Restrictions)
    , _dcLogging              :: !(Maybe LoggingConfig)
    , _dcCacheBehaviors       :: !(Maybe CacheBehaviors)
    , _dcCallerReference      :: !Text
    , _dcOrigins              :: !Origins
    , _dcDefaultCacheBehavior :: !DefaultCacheBehavior
    , _dcComment              :: !Text
    , _dcEnabled              :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcAliases'
--
-- * 'dcDefaultRootObject'
--
-- * 'dcPriceClass'
--
-- * 'dcCustomErrorResponses'
--
-- * 'dcWebACLId'
--
-- * 'dcViewerCertificate'
--
-- * 'dcRestrictions'
--
-- * 'dcLogging'
--
-- * 'dcCacheBehaviors'
--
-- * 'dcCallerReference'
--
-- * 'dcOrigins'
--
-- * 'dcDefaultCacheBehavior'
--
-- * 'dcComment'
--
-- * 'dcEnabled'
distributionConfig
    :: Text -- ^ 'dcCallerReference'
    -> Origins -- ^ 'dcOrigins'
    -> DefaultCacheBehavior -- ^ 'dcDefaultCacheBehavior'
    -> Text -- ^ 'dcComment'
    -> Bool -- ^ 'dcEnabled'
    -> DistributionConfig
distributionConfig pCallerReference_ pOrigins_ pDefaultCacheBehavior_ pComment_ pEnabled_ =
    DistributionConfig'
    { _dcAliases = Nothing
    , _dcDefaultRootObject = Nothing
    , _dcPriceClass = Nothing
    , _dcCustomErrorResponses = Nothing
    , _dcWebACLId = Nothing
    , _dcViewerCertificate = Nothing
    , _dcRestrictions = Nothing
    , _dcLogging = Nothing
    , _dcCacheBehaviors = Nothing
    , _dcCallerReference = pCallerReference_
    , _dcOrigins = pOrigins_
    , _dcDefaultCacheBehavior = pDefaultCacheBehavior_
    , _dcComment = pComment_
    , _dcEnabled = pEnabled_
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Maybe Aliases)
dcAliases = lens _dcAliases (\ s a -> s{_dcAliases = a});

-- | The object that you want CloudFront to return (for example, index.html)
-- when an end user requests the root URL for your distribution
-- (http:\/\/www.example.com) instead of an object in your distribution
-- (http:\/\/www.example.com\/index.html). Specifying a default root object
-- avoids exposing the contents of your distribution. If you don\'t want to
-- specify a default root object when you create a distribution, include an
-- empty DefaultRootObject element. To delete the default root object from
-- an existing distribution, update the distribution configuration and
-- include an empty DefaultRootObject element. To replace the default root
-- object, update the distribution configuration and specify the new
-- object.
dcDefaultRootObject :: Lens' DistributionConfig (Maybe Text)
dcDefaultRootObject = lens _dcDefaultRootObject (\ s a -> s{_dcDefaultRootObject = a});

-- | A complex type that contains information about price class for this
-- distribution.
dcPriceClass :: Lens' DistributionConfig (Maybe PriceClass)
dcPriceClass = lens _dcPriceClass (\ s a -> s{_dcPriceClass = a});

-- | A complex type that contains zero or more CustomErrorResponse elements.
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses = lens _dcCustomErrorResponses (\ s a -> s{_dcCustomErrorResponses = a});

-- | (Optional) If you\'re using AWS WAF to filter CloudFront requests, the
-- Id of the AWS WAF web ACL that is associated with the distribution.
dcWebACLId :: Lens' DistributionConfig (Maybe Text)
dcWebACLId = lens _dcWebACLId (\ s a -> s{_dcWebACLId = a});

-- | Undocumented member.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate = lens _dcViewerCertificate (\ s a -> s{_dcViewerCertificate = a});

-- | Undocumented member.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\ s a -> s{_dcRestrictions = a});

-- | A complex type that controls whether access logs are written for the
-- distribution.
dcLogging :: Lens' DistributionConfig (Maybe LoggingConfig)
dcLogging = lens _dcLogging (\ s a -> s{_dcLogging = a});

-- | A complex type that contains zero or more CacheBehavior elements.
dcCacheBehaviors :: Lens' DistributionConfig (Maybe CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\ s a -> s{_dcCacheBehaviors = a});

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the DistributionConfig
-- object), a new distribution is created. If the CallerReference is a
-- value you already sent in a previous request to create a distribution,
-- and the content of the DistributionConfig is identical to the original
-- request (ignoring white space), the response includes the same
-- information returned to the original request. If the CallerReference is
-- a value you already sent in a previous request to create a distribution
-- but the content of the DistributionConfig is different from the original
-- request, CloudFront returns a DistributionAlreadyExists error.
dcCallerReference :: Lens' DistributionConfig Text
dcCallerReference = lens _dcCallerReference (\ s a -> s{_dcCallerReference = a});

-- | A complex type that contains information about origins for this
-- distribution.
dcOrigins :: Lens' DistributionConfig Origins
dcOrigins = lens _dcOrigins (\ s a -> s{_dcOrigins = a});

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior = lens _dcDefaultCacheBehavior (\ s a -> s{_dcDefaultCacheBehavior = a});

-- | Any comments you want to include about the distribution.
dcComment :: Lens' DistributionConfig Text
dcComment = lens _dcComment (\ s a -> s{_dcComment = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dcEnabled :: Lens' DistributionConfig Bool
dcEnabled = lens _dcEnabled (\ s a -> s{_dcEnabled = a});

instance FromXML DistributionConfig where
        parseXML x
          = DistributionConfig' <$>
              (x .@? "Aliases") <*> (x .@? "DefaultRootObject") <*>
                (x .@? "PriceClass")
                <*> (x .@? "CustomErrorResponses")
                <*> (x .@? "WebACLId")
                <*> (x .@? "ViewerCertificate")
                <*> (x .@? "Restrictions")
                <*> (x .@? "Logging")
                <*> (x .@? "CacheBehaviors")
                <*> (x .@ "CallerReference")
                <*> (x .@ "Origins")
                <*> (x .@ "DefaultCacheBehavior")
                <*> (x .@ "Comment")
                <*> (x .@ "Enabled")

instance ToXML DistributionConfig where
        toXML DistributionConfig'{..}
          = mconcat
              ["Aliases" @= _dcAliases,
               "DefaultRootObject" @= _dcDefaultRootObject,
               "PriceClass" @= _dcPriceClass,
               "CustomErrorResponses" @= _dcCustomErrorResponses,
               "WebACLId" @= _dcWebACLId,
               "ViewerCertificate" @= _dcViewerCertificate,
               "Restrictions" @= _dcRestrictions,
               "Logging" @= _dcLogging,
               "CacheBehaviors" @= _dcCacheBehaviors,
               "CallerReference" @= _dcCallerReference,
               "Origins" @= _dcOrigins,
               "DefaultCacheBehavior" @= _dcDefaultCacheBehavior,
               "Comment" @= _dcComment, "Enabled" @= _dcEnabled]

-- | A distribution list.
--
-- /See:/ 'distributionList' smart constructor.
data DistributionList = DistributionList'
    { _dlItems       :: !(Maybe [DistributionSummary])
    , _dlNextMarker  :: !(Maybe Text)
    , _dlMarker      :: !Text
    , _dlMaxItems    :: !Int
    , _dlIsTruncated :: !Bool
    , _dlQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlItems'
--
-- * 'dlNextMarker'
--
-- * 'dlMarker'
--
-- * 'dlMaxItems'
--
-- * 'dlIsTruncated'
--
-- * 'dlQuantity'
distributionList
    :: Text -- ^ 'dlMarker'
    -> Int -- ^ 'dlMaxItems'
    -> Bool -- ^ 'dlIsTruncated'
    -> Int -- ^ 'dlQuantity'
    -> DistributionList
distributionList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
    DistributionList'
    { _dlItems = Nothing
    , _dlNextMarker = Nothing
    , _dlMarker = pMarker_
    , _dlMaxItems = pMaxItems_
    , _dlIsTruncated = pIsTruncated_
    , _dlQuantity = pQuantity_
    }

-- | A complex type that contains one DistributionSummary element for each
-- distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList [DistributionSummary]
dlItems = lens _dlItems (\ s a -> s{_dlItems = a}) . _Default . _Coerce;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\ s a -> s{_dlNextMarker = a});

-- | The value you provided for the Marker request parameter.
dlMarker :: Lens' DistributionList Text
dlMarker = lens _dlMarker (\ s a -> s{_dlMarker = a});

-- | The value you provided for the MaxItems request parameter.
dlMaxItems :: Lens' DistributionList Int
dlMaxItems = lens _dlMaxItems (\ s a -> s{_dlMaxItems = a});

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the Marker request parameter to retrieve more distributions in the
-- list.
dlIsTruncated :: Lens' DistributionList Bool
dlIsTruncated = lens _dlIsTruncated (\ s a -> s{_dlIsTruncated = a});

-- | The number of distributions that were created by the current AWS
-- account.
dlQuantity :: Lens' DistributionList Int
dlQuantity = lens _dlQuantity (\ s a -> s{_dlQuantity = a});

instance FromXML DistributionList where
        parseXML x
          = DistributionList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "DistributionSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | A summary of the information for an Amazon CloudFront distribution.
--
-- /See:/ 'distributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
    { _dsId                   :: !Text
    , _dsStatus               :: !Text
    , _dsLastModifiedTime     :: !ISO8601
    , _dsDomainName           :: !Text
    , _dsAliases              :: !Aliases
    , _dsOrigins              :: !Origins
    , _dsDefaultCacheBehavior :: !DefaultCacheBehavior
    , _dsCacheBehaviors       :: !CacheBehaviors
    , _dsCustomErrorResponses :: !CustomErrorResponses
    , _dsComment              :: !Text
    , _dsPriceClass           :: !PriceClass
    , _dsEnabled              :: !Bool
    , _dsViewerCertificate    :: !ViewerCertificate
    , _dsRestrictions         :: !Restrictions
    , _dsWebACLId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId'
--
-- * 'dsStatus'
--
-- * 'dsLastModifiedTime'
--
-- * 'dsDomainName'
--
-- * 'dsAliases'
--
-- * 'dsOrigins'
--
-- * 'dsDefaultCacheBehavior'
--
-- * 'dsCacheBehaviors'
--
-- * 'dsCustomErrorResponses'
--
-- * 'dsComment'
--
-- * 'dsPriceClass'
--
-- * 'dsEnabled'
--
-- * 'dsViewerCertificate'
--
-- * 'dsRestrictions'
--
-- * 'dsWebACLId'
distributionSummary
    :: Text -- ^ 'dsId'
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
    -> Text -- ^ 'dsWebACLId'
    -> DistributionSummary
distributionSummary pId_ pStatus_ pLastModifiedTime_ pDomainName_ pAliases_ pOrigins_ pDefaultCacheBehavior_ pCacheBehaviors_ pCustomErrorResponses_ pComment_ pPriceClass_ pEnabled_ pViewerCertificate_ pRestrictions_ pWebACLId_ =
    DistributionSummary'
    { _dsId = pId_
    , _dsStatus = pStatus_
    , _dsLastModifiedTime = _Time # pLastModifiedTime_
    , _dsDomainName = pDomainName_
    , _dsAliases = pAliases_
    , _dsOrigins = pOrigins_
    , _dsDefaultCacheBehavior = pDefaultCacheBehavior_
    , _dsCacheBehaviors = pCacheBehaviors_
    , _dsCustomErrorResponses = pCustomErrorResponses_
    , _dsComment = pComment_
    , _dsPriceClass = pPriceClass_
    , _dsEnabled = pEnabled_
    , _dsViewerCertificate = pViewerCertificate_
    , _dsRestrictions = pRestrictions_
    , _dsWebACLId = pWebACLId_
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dsId :: Lens' DistributionSummary Text
dsId = lens _dsId (\ s a -> s{_dsId = a});

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution\'s information is fully
-- propagated throughout the Amazon CloudFront system.
dsStatus :: Lens' DistributionSummary Text
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a});

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary UTCTime
dsLastModifiedTime = lens _dsLastModifiedTime (\ s a -> s{_dsLastModifiedTime = a}) . _Time;

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dsDomainName :: Lens' DistributionSummary Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a});

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary Aliases
dsAliases = lens _dsAliases (\ s a -> s{_dsAliases = a});

-- | A complex type that contains information about origins for this
-- distribution.
dsOrigins :: Lens' DistributionSummary Origins
dsOrigins = lens _dsOrigins (\ s a -> s{_dsOrigins = a});

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior = lens _dsDefaultCacheBehavior (\ s a -> s{_dsDefaultCacheBehavior = a});

-- | A complex type that contains zero or more CacheBehavior elements.
dsCacheBehaviors :: Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = lens _dsCacheBehaviors (\ s a -> s{_dsCacheBehaviors = a});

-- | A complex type that contains zero or more CustomErrorResponses elements.
dsCustomErrorResponses :: Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses = lens _dsCustomErrorResponses (\ s a -> s{_dsCustomErrorResponses = a});

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary Text
dsComment = lens _dsComment (\ s a -> s{_dsComment = a});

-- | Undocumented member.
dsPriceClass :: Lens' DistributionSummary PriceClass
dsPriceClass = lens _dsPriceClass (\ s a -> s{_dsPriceClass = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dsEnabled :: Lens' DistributionSummary Bool
dsEnabled = lens _dsEnabled (\ s a -> s{_dsEnabled = a});

-- | Undocumented member.
dsViewerCertificate :: Lens' DistributionSummary ViewerCertificate
dsViewerCertificate = lens _dsViewerCertificate (\ s a -> s{_dsViewerCertificate = a});

-- | Undocumented member.
dsRestrictions :: Lens' DistributionSummary Restrictions
dsRestrictions = lens _dsRestrictions (\ s a -> s{_dsRestrictions = a});

-- | The Web ACL Id (if any) associated with the distribution.
dsWebACLId :: Lens' DistributionSummary Text
dsWebACLId = lens _dsWebACLId (\ s a -> s{_dsWebACLId = a});

instance FromXML DistributionSummary where
        parseXML x
          = DistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "DomainName")
                <*> (x .@ "Aliases")
                <*> (x .@ "Origins")
                <*> (x .@ "DefaultCacheBehavior")
                <*> (x .@ "CacheBehaviors")
                <*> (x .@ "CustomErrorResponses")
                <*> (x .@ "Comment")
                <*> (x .@ "PriceClass")
                <*> (x .@ "Enabled")
                <*> (x .@ "ViewerCertificate")
                <*> (x .@ "Restrictions")
                <*> (x .@ "WebACLId")

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
--
-- /See:/ 'forwardedValues' smart constructor.
data ForwardedValues = ForwardedValues'
    { _fvHeaders     :: !(Maybe Headers)
    , _fvQueryString :: !Bool
    , _fvCookies     :: !CookiePreference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ForwardedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fvHeaders'
--
-- * 'fvQueryString'
--
-- * 'fvCookies'
forwardedValues
    :: Bool -- ^ 'fvQueryString'
    -> CookiePreference -- ^ 'fvCookies'
    -> ForwardedValues
forwardedValues pQueryString_ pCookies_ =
    ForwardedValues'
    { _fvHeaders = Nothing
    , _fvQueryString = pQueryString_
    , _fvCookies = pCookies_
    }

-- | A complex type that specifies the Headers, if any, that you want
-- CloudFront to vary upon for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\ s a -> s{_fvHeaders = a});

-- | Indicates whether you want CloudFront to forward query strings to the
-- origin that is associated with this cache behavior. If so, specify true;
-- if not, specify false.
fvQueryString :: Lens' ForwardedValues Bool
fvQueryString = lens _fvQueryString (\ s a -> s{_fvQueryString = a});

-- | A complex type that specifies how CloudFront handles cookies.
fvCookies :: Lens' ForwardedValues CookiePreference
fvCookies = lens _fvCookies (\ s a -> s{_fvCookies = a});

instance FromXML ForwardedValues where
        parseXML x
          = ForwardedValues' <$>
              (x .@? "Headers") <*> (x .@ "QueryString") <*>
                (x .@ "Cookies")

instance ToXML ForwardedValues where
        toXML ForwardedValues'{..}
          = mconcat
              ["Headers" @= _fvHeaders,
               "QueryString" @= _fvQueryString,
               "Cookies" @= _fvCookies]

-- | A complex type that controls the countries in which your content is
-- distributed. For more information about geo restriction, go to
-- Customizing Error Responses in the Amazon CloudFront Developer Guide.
-- CloudFront determines the location of your users using MaxMind GeoIP
-- databases. For information about the accuracy of these databases, see
-- How accurate are your GeoIP databases? on the MaxMind website.
--
-- /See:/ 'geoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
    { _grItems           :: !(Maybe [Text])
    , _grRestrictionType :: !GeoRestrictionType
    , _grQuantity        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GeoRestriction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grItems'
--
-- * 'grRestrictionType'
--
-- * 'grQuantity'
geoRestriction
    :: GeoRestrictionType -- ^ 'grRestrictionType'
    -> Int -- ^ 'grQuantity'
    -> GeoRestriction
geoRestriction pRestrictionType_ pQuantity_ =
    GeoRestriction'
    { _grItems = Nothing
    , _grRestrictionType = pRestrictionType_
    , _grQuantity = pQuantity_
    }

-- | A complex type that contains a Location element for each country in
-- which you want CloudFront either to distribute your content (whitelist)
-- or not distribute your content (blacklist). The Location element is a
-- two-letter, uppercase country code for a country that you want to
-- include in your blacklist or whitelist. Include one Location element for
-- each country. CloudFront and MaxMind both use ISO 3166 country codes.
-- For the current list of countries and the corresponding codes, see ISO
-- 3166-1-alpha-2 code on the International Organization for
-- Standardization website. You can also refer to the country list in the
-- CloudFront console, which includes both country names and codes.
grItems :: Lens' GeoRestriction [Text]
grItems = lens _grItems (\ s a -> s{_grItems = a}) . _Default . _Coerce;

-- | The method that you want to use to restrict distribution of your content
-- by country: - none: No geo restriction is enabled, meaning access to
-- content is not restricted by client geo location. - blacklist: The
-- Location elements specify the countries in which you do not want
-- CloudFront to distribute your content. - whitelist: The Location
-- elements specify the countries in which you want CloudFront to
-- distribute your content.
grRestrictionType :: Lens' GeoRestriction GeoRestrictionType
grRestrictionType = lens _grRestrictionType (\ s a -> s{_grRestrictionType = a});

-- | When geo restriction is enabled, this is the number of countries in your
-- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is
-- 0, and you can omit Items.
grQuantity :: Lens' GeoRestriction Int
grQuantity = lens _grQuantity (\ s a -> s{_grQuantity = a});

instance FromXML GeoRestriction where
        parseXML x
          = GeoRestriction' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Location"))
                <*> (x .@ "RestrictionType")
                <*> (x .@ "Quantity")

instance ToXML GeoRestriction where
        toXML GeoRestriction'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "Location" <$> _grItems),
               "RestrictionType" @= _grRestrictionType,
               "Quantity" @= _grQuantity]

-- | A complex type that specifies the headers that you want CloudFront to
-- forward to the origin for this cache behavior. For the headers that you
-- specify, CloudFront also caches separate versions of a given object
-- based on the header values in viewer requests; this is known as varying
-- on headers. For example, suppose viewer requests for logo.jpg contain a
-- custom Product header that has a value of either Acme or Apex, and you
-- configure CloudFront to vary on the Product header. CloudFront forwards
-- the Product header to the origin and caches the response from the origin
-- once for each header value.
--
-- /See:/ 'headers' smart constructor.
data Headers = Headers'
    { _hItems    :: !(Maybe [Text])
    , _hQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hItems'
--
-- * 'hQuantity'
headers
    :: Int -- ^ 'hQuantity'
    -> Headers
headers pQuantity_ =
    Headers'
    { _hItems = Nothing
    , _hQuantity = pQuantity_
    }

-- | Optional: A complex type that contains a Name element for each header
-- that you want CloudFront to forward to the origin and to vary on for
-- this cache behavior. If Quantity is 0, omit Items.
hItems :: Lens' Headers [Text]
hItems = lens _hItems (\ s a -> s{_hItems = a}) . _Default . _Coerce;

-- | The number of different headers that you want CloudFront to forward to
-- the origin and to vary on for this cache behavior. The maximum number of
-- headers that you can specify by name is 10. If you want CloudFront to
-- forward all headers to the origin and vary on all of them, specify 1 for
-- Quantity and * for Name. If you don\'t want CloudFront to forward any
-- additional headers to the origin or to vary on any headers, specify 0
-- for Quantity and omit Items.
hQuantity :: Lens' Headers Int
hQuantity = lens _hQuantity (\ s a -> s{_hQuantity = a});

instance FromXML Headers where
        parseXML x
          = Headers' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance ToXML Headers where
        toXML Headers'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _hItems),
               "Quantity" @= _hQuantity]

-- | An invalidation.
--
-- /See:/ 'invalidation' smart constructor.
data Invalidation = Invalidation'
    { _iId                :: !Text
    , _iStatus            :: !Text
    , _iCreateTime        :: !ISO8601
    , _iInvalidationBatch :: !InvalidationBatch
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Invalidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId'
--
-- * 'iStatus'
--
-- * 'iCreateTime'
--
-- * 'iInvalidationBatch'
invalidation
    :: Text -- ^ 'iId'
    -> Text -- ^ 'iStatus'
    -> UTCTime -- ^ 'iCreateTime'
    -> InvalidationBatch -- ^ 'iInvalidationBatch'
    -> Invalidation
invalidation pId_ pStatus_ pCreateTime_ pInvalidationBatch_ =
    Invalidation'
    { _iId = pId_
    , _iStatus = pStatus_
    , _iCreateTime = _Time # pCreateTime_
    , _iInvalidationBatch = pInvalidationBatch_
    }

-- | The identifier for the invalidation request. For example:
-- IDFDVBD632BHDS5.
iId :: Lens' Invalidation Text
iId = lens _iId (\ s a -> s{_iId = a});

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is Completed.
iStatus :: Lens' Invalidation Text
iStatus = lens _iStatus (\ s a -> s{_iStatus = a});

-- | The date and time the invalidation request was first made.
iCreateTime :: Lens' Invalidation UTCTime
iCreateTime = lens _iCreateTime (\ s a -> s{_iCreateTime = a}) . _Time;

-- | The current invalidation information for the batch request.
iInvalidationBatch :: Lens' Invalidation InvalidationBatch
iInvalidationBatch = lens _iInvalidationBatch (\ s a -> s{_iInvalidationBatch = a});

instance FromXML Invalidation where
        parseXML x
          = Invalidation' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "CreateTime")
                <*> (x .@ "InvalidationBatch")

-- | An invalidation batch.
--
-- /See:/ 'invalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
    { _ibPaths           :: !Paths
    , _ibCallerReference :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InvalidationBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibPaths'
--
-- * 'ibCallerReference'
invalidationBatch
    :: Paths -- ^ 'ibPaths'
    -> Text -- ^ 'ibCallerReference'
    -> InvalidationBatch
invalidationBatch pPaths_ pCallerReference_ =
    InvalidationBatch'
    { _ibPaths = pPaths_
    , _ibCallerReference = pCallerReference_
    }

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (\/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http:\/\/www.ietf.org\/rfc\/rfc1738.txt), URL encode those characters.
-- Do not URL encode any other characters in the path, or CloudFront will
-- not invalidate the old version of the updated object.
ibPaths :: Lens' InvalidationBatch Paths
ibPaths = lens _ibPaths (\ s a -> s{_ibPaths = a});

-- | A unique name that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the Path object), a new
-- distribution is created. If the CallerReference is a value you already
-- sent in a previous request to create an invalidation batch, and the
-- content of each Path element is identical to the original request, the
-- response includes the same information returned to the original request.
-- If the CallerReference is a value you already sent in a previous request
-- to create a distribution but the content of any Path is different from
-- the original request, CloudFront returns an
-- InvalidationBatchAlreadyExists error.
ibCallerReference :: Lens' InvalidationBatch Text
ibCallerReference = lens _ibCallerReference (\ s a -> s{_ibCallerReference = a});

instance FromXML InvalidationBatch where
        parseXML x
          = InvalidationBatch' <$>
              (x .@ "Paths") <*> (x .@ "CallerReference")

instance ToXML InvalidationBatch where
        toXML InvalidationBatch'{..}
          = mconcat
              ["Paths" @= _ibPaths,
               "CallerReference" @= _ibCallerReference]

-- | An invalidation list.
--
-- /See:/ 'invalidationList' smart constructor.
data InvalidationList = InvalidationList'
    { _ilItems       :: !(Maybe [InvalidationSummary])
    , _ilNextMarker  :: !(Maybe Text)
    , _ilMarker      :: !Text
    , _ilMaxItems    :: !Int
    , _ilIsTruncated :: !Bool
    , _ilQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InvalidationList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilItems'
--
-- * 'ilNextMarker'
--
-- * 'ilMarker'
--
-- * 'ilMaxItems'
--
-- * 'ilIsTruncated'
--
-- * 'ilQuantity'
invalidationList
    :: Text -- ^ 'ilMarker'
    -> Int -- ^ 'ilMaxItems'
    -> Bool -- ^ 'ilIsTruncated'
    -> Int -- ^ 'ilQuantity'
    -> InvalidationList
invalidationList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
    InvalidationList'
    { _ilItems = Nothing
    , _ilNextMarker = Nothing
    , _ilMarker = pMarker_
    , _ilMaxItems = pMaxItems_
    , _ilIsTruncated = pIsTruncated_
    , _ilQuantity = pQuantity_
    }

-- | A complex type that contains one InvalidationSummary element for each
-- invalidation batch that was created by the current AWS account.
ilItems :: Lens' InvalidationList [InvalidationSummary]
ilItems = lens _ilItems (\ s a -> s{_ilItems = a}) . _Default . _Coerce;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\ s a -> s{_ilNextMarker = a});

-- | The value you provided for the Marker request parameter.
ilMarker :: Lens' InvalidationList Text
ilMarker = lens _ilMarker (\ s a -> s{_ilMarker = a});

-- | The value you provided for the MaxItems request parameter.
ilMaxItems :: Lens' InvalidationList Int
ilMaxItems = lens _ilMaxItems (\ s a -> s{_ilMaxItems = a});

-- | A flag that indicates whether more invalidation batch requests remain to
-- be listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- invalidation batches in the list.
ilIsTruncated :: Lens' InvalidationList Bool
ilIsTruncated = lens _ilIsTruncated (\ s a -> s{_ilIsTruncated = a});

-- | The number of invalidation batches that were created by the current AWS
-- account.
ilQuantity :: Lens' InvalidationList Int
ilQuantity = lens _ilQuantity (\ s a -> s{_ilQuantity = a});

instance FromXML InvalidationList where
        parseXML x
          = InvalidationList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "InvalidationSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | Summary of an invalidation request.
--
-- /See:/ 'invalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
    { _isId         :: !Text
    , _isCreateTime :: !ISO8601
    , _isStatus     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InvalidationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isId'
--
-- * 'isCreateTime'
--
-- * 'isStatus'
invalidationSummary
    :: Text -- ^ 'isId'
    -> UTCTime -- ^ 'isCreateTime'
    -> Text -- ^ 'isStatus'
    -> InvalidationSummary
invalidationSummary pId_ pCreateTime_ pStatus_ =
    InvalidationSummary'
    { _isId = pId_
    , _isCreateTime = _Time # pCreateTime_
    , _isStatus = pStatus_
    }

-- | The unique ID for an invalidation request.
isId :: Lens' InvalidationSummary Text
isId = lens _isId (\ s a -> s{_isId = a});

-- | Undocumented member.
isCreateTime :: Lens' InvalidationSummary UTCTime
isCreateTime = lens _isCreateTime (\ s a -> s{_isCreateTime = a}) . _Time;

-- | The status of an invalidation request.
isStatus :: Lens' InvalidationSummary Text
isStatus = lens _isStatus (\ s a -> s{_isStatus = a});

instance FromXML InvalidationSummary where
        parseXML x
          = InvalidationSummary' <$>
              (x .@ "Id") <*> (x .@ "CreateTime") <*>
                (x .@ "Status")

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
--
-- /See:/ 'keyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
    { _kpiItems    :: !(Maybe [Text])
    , _kpiQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'KeyPairIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiItems'
--
-- * 'kpiQuantity'
keyPairIds
    :: Int -- ^ 'kpiQuantity'
    -> KeyPairIds
keyPairIds pQuantity_ =
    KeyPairIds'
    { _kpiItems = Nothing
    , _kpiQuantity = pQuantity_
    }

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\ s a -> s{_kpiItems = a}) . _Default . _Coerce;

-- | The number of active CloudFront key pairs for AwsAccountNumber.
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\ s a -> s{_kpiQuantity = a});

instance FromXML KeyPairIds where
        parseXML x
          = KeyPairIds' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "KeyPairId"))
                <*> (x .@ "Quantity")

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- /See:/ 'loggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
    { _lcEnabled        :: !Bool
    , _lcIncludeCookies :: !Bool
    , _lcBucket         :: !Text
    , _lcPrefix         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcEnabled'
--
-- * 'lcIncludeCookies'
--
-- * 'lcBucket'
--
-- * 'lcPrefix'
loggingConfig
    :: Bool -- ^ 'lcEnabled'
    -> Bool -- ^ 'lcIncludeCookies'
    -> Text -- ^ 'lcBucket'
    -> Text -- ^ 'lcPrefix'
    -> LoggingConfig
loggingConfig pEnabled_ pIncludeCookies_ pBucket_ pPrefix_ =
    LoggingConfig'
    { _lcEnabled = pEnabled_
    , _lcIncludeCookies = pIncludeCookies_
    , _lcBucket = pBucket_
    , _lcPrefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you do not want to enable logging when you create a
-- distribution or if you want to disable logging for an existing
-- distribution, specify false for Enabled, and specify empty Bucket and
-- Prefix elements. If you specify false for Enabled but you specify values
-- for Bucket, prefix and IncludeCookies, the values are automatically
-- deleted.
lcEnabled :: Lens' LoggingConfig Bool
lcEnabled = lens _lcEnabled (\ s a -> s{_lcEnabled = a});

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify true for IncludeCookies. If you choose to include cookies in
-- logs, CloudFront logs all cookies regardless of how you configure the
-- cache behaviors for this distribution. If you do not want to include
-- cookies when you create a distribution or if you want to disable include
-- cookies for an existing distribution, specify false for IncludeCookies.
lcIncludeCookies :: Lens' LoggingConfig Bool
lcIncludeCookies = lens _lcIncludeCookies (\ s a -> s{_lcIncludeCookies = a});

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
lcBucket :: Lens' LoggingConfig Text
lcBucket = lens _lcBucket (\ s a -> s{_lcBucket = a});

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this distribution, for example, myprefix\/. If you want to
-- enable logging, but you do not want to specify a prefix, you still must
-- include an empty Prefix element in the Logging element.
lcPrefix :: Lens' LoggingConfig Text
lcPrefix = lens _lcPrefix (\ s a -> s{_lcPrefix = a});

instance FromXML LoggingConfig where
        parseXML x
          = LoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "IncludeCookies") <*>
                (x .@ "Bucket")
                <*> (x .@ "Prefix")

instance ToXML LoggingConfig where
        toXML LoggingConfig'{..}
          = mconcat
              ["Enabled" @= _lcEnabled,
               "IncludeCookies" @= _lcIncludeCookies,
               "Bucket" @= _lcBucket, "Prefix" @= _lcPrefix]

-- | A complex type that describes the Amazon S3 bucket or the HTTP server
-- (for example, a web server) from which CloudFront gets your files.You
-- must create at least one origin.
--
-- /See:/ 'origin' smart constructor.
data Origin = Origin'
    { _oCustomOriginConfig :: !(Maybe CustomOriginConfig)
    , _oS3OriginConfig     :: !(Maybe S3OriginConfig)
    , _oOriginPath         :: !(Maybe Text)
    , _oId                 :: !Text
    , _oDomainName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCustomOriginConfig'
--
-- * 'oS3OriginConfig'
--
-- * 'oOriginPath'
--
-- * 'oId'
--
-- * 'oDomainName'
origin
    :: Text -- ^ 'oId'
    -> Text -- ^ 'oDomainName'
    -> Origin
origin pId_ pDomainName_ =
    Origin'
    { _oCustomOriginConfig = Nothing
    , _oS3OriginConfig = Nothing
    , _oOriginPath = Nothing
    , _oId = pId_
    , _oDomainName = pDomainName_
    }

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
oCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oCustomOriginConfig = lens _oCustomOriginConfig (\ s a -> s{_oCustomOriginConfig = a});

-- | A complex type that contains information about the Amazon S3 origin. If
-- the origin is a custom origin, use the CustomOriginConfig element
-- instead.
oS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oS3OriginConfig = lens _oS3OriginConfig (\ s a -> s{_oS3OriginConfig = a});

-- | An optional element that causes CloudFront to request your content from
-- a directory in your Amazon S3 bucket or your custom origin. When you
-- include the OriginPath element, specify the directory name, beginning
-- with a \/. CloudFront appends the directory name to the value of
-- DomainName.
oOriginPath :: Lens' Origin (Maybe Text)
oOriginPath = lens _oOriginPath (\ s a -> s{_oOriginPath = a});

-- | A unique identifier for the origin. The value of Id must be unique
-- within the distribution. You use the value of Id when you create a cache
-- behavior. The Id identifies the origin that CloudFront routes a request
-- to when the request matches the path pattern for that cache behavior.
oId :: Lens' Origin Text
oId = lens _oId (\ s a -> s{_oId = a});

-- | Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you
-- want CloudFront to get objects for this origin, for example,
-- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for
-- the HTTP server from which you want CloudFront to get objects for this
-- origin, for example, www.example.com.
oDomainName :: Lens' Origin Text
oDomainName = lens _oDomainName (\ s a -> s{_oDomainName = a});

instance FromXML Origin where
        parseXML x
          = Origin' <$>
              (x .@? "CustomOriginConfig") <*>
                (x .@? "S3OriginConfig")
                <*> (x .@? "OriginPath")
                <*> (x .@ "Id")
                <*> (x .@ "DomainName")

instance ToXML Origin where
        toXML Origin'{..}
          = mconcat
              ["CustomOriginConfig" @= _oCustomOriginConfig,
               "S3OriginConfig" @= _oS3OriginConfig,
               "OriginPath" @= _oOriginPath, "Id" @= _oId,
               "DomainName" @= _oDomainName]

-- | A complex type that contains information about origins for this
-- distribution.
--
-- /See:/ 'origins' smart constructor.
data Origins = Origins'
    { _oItems    :: !(Maybe (List1 Origin))
    , _oQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Origins' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oItems'
--
-- * 'oQuantity'
origins
    :: Int -- ^ 'oQuantity'
    -> Origins
origins pQuantity_ =
    Origins'
    { _oItems = Nothing
    , _oQuantity = pQuantity_
    }

-- | A complex type that contains origins for this distribution.
oItems :: Lens' Origins (Maybe (NonEmpty Origin))
oItems = lens _oItems (\ s a -> s{_oItems = a}) . mapping _List1;

-- | The number of origins for this distribution.
oQuantity :: Lens' Origins Int
oQuantity = lens _oQuantity (\ s a -> s{_oQuantity = a});

instance FromXML Origins where
        parseXML x
          = Origins' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList1 "Origin"))
                <*> (x .@ "Quantity")

instance ToXML Origins where
        toXML Origins'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Origin" <$> _oItems),
               "Quantity" @= _oQuantity]

-- | A complex type that contains information about the objects that you want
-- to invalidate.
--
-- /See:/ 'paths' smart constructor.
data Paths = Paths'
    { _pItems    :: !(Maybe [Text])
    , _pQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Paths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pItems'
--
-- * 'pQuantity'
paths
    :: Int -- ^ 'pQuantity'
    -> Paths
paths pQuantity_ =
    Paths'
    { _pItems = Nothing
    , _pQuantity = pQuantity_
    }

-- | A complex type that contains a list of the objects that you want to
-- invalidate.
pItems :: Lens' Paths [Text]
pItems = lens _pItems (\ s a -> s{_pItems = a}) . _Default . _Coerce;

-- | The number of objects that you want to invalidate.
pQuantity :: Lens' Paths Int
pQuantity = lens _pQuantity (\ s a -> s{_pQuantity = a});

instance FromXML Paths where
        parseXML x
          = Paths' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Path"))
                <*> (x .@ "Quantity")

instance ToXML Paths where
        toXML Paths'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Path" <$> _pItems),
               "Quantity" @= _pQuantity]

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- /See:/ 'restrictions' smart constructor.
newtype Restrictions = Restrictions'
    { _rGeoRestriction :: GeoRestriction
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Restrictions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rGeoRestriction'
restrictions
    :: GeoRestriction -- ^ 'rGeoRestriction'
    -> Restrictions
restrictions pGeoRestriction_ =
    Restrictions'
    { _rGeoRestriction = pGeoRestriction_
    }

-- | Undocumented member.
rGeoRestriction :: Lens' Restrictions GeoRestriction
rGeoRestriction = lens _rGeoRestriction (\ s a -> s{_rGeoRestriction = a});

instance FromXML Restrictions where
        parseXML x
          = Restrictions' <$> (x .@ "GeoRestriction")

instance ToXML Restrictions where
        toXML Restrictions'{..}
          = mconcat ["GeoRestriction" @= _rGeoRestriction]

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
--
-- /See:/ 's3Origin' smart constructor.
data S3Origin = S3Origin'
    { _soDomainName           :: !Text
    , _soOriginAccessIdentity :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soDomainName'
--
-- * 'soOriginAccessIdentity'
s3Origin
    :: Text -- ^ 'soDomainName'
    -> Text -- ^ 'soOriginAccessIdentity'
    -> S3Origin
s3Origin pDomainName_ pOriginAccessIdentity_ =
    S3Origin'
    { _soDomainName = pDomainName_
    , _soOriginAccessIdentity = pOriginAccessIdentity_
    }

-- | The DNS name of the S3 origin.
soDomainName :: Lens' S3Origin Text
soDomainName = lens _soDomainName (\ s a -> s{_soDomainName = a});

-- | Your S3 origin\'s origin access identity.
soOriginAccessIdentity :: Lens' S3Origin Text
soOriginAccessIdentity = lens _soOriginAccessIdentity (\ s a -> s{_soOriginAccessIdentity = a});

instance FromXML S3Origin where
        parseXML x
          = S3Origin' <$>
              (x .@ "DomainName") <*> (x .@ "OriginAccessIdentity")

instance ToXML S3Origin where
        toXML S3Origin'{..}
          = mconcat
              ["DomainName" @= _soDomainName,
               "OriginAccessIdentity" @= _soOriginAccessIdentity]

-- | A complex type that contains information about the Amazon S3 origin. If
-- the origin is a custom origin, use the CustomOriginConfig element
-- instead.
--
-- /See:/ 's3OriginConfig' smart constructor.
newtype S3OriginConfig = S3OriginConfig'
    { _socOriginAccessIdentity :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3OriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socOriginAccessIdentity'
s3OriginConfig
    :: Text -- ^ 'socOriginAccessIdentity'
    -> S3OriginConfig
s3OriginConfig pOriginAccessIdentity_ =
    S3OriginConfig'
    { _socOriginAccessIdentity = pOriginAccessIdentity_
    }

-- | The CloudFront origin access identity to associate with the origin. Use
-- an origin access identity to configure the origin so that end users can
-- only access objects in an Amazon S3 bucket through CloudFront. If you
-- want end users to be able to access objects using either the CloudFront
-- URL or the Amazon S3 URL, specify an empty OriginAccessIdentity element.
-- To delete the origin access identity from an existing distribution,
-- update the distribution configuration and include an empty
-- OriginAccessIdentity element. To replace the origin access identity,
-- update the distribution configuration and specify the new origin access
-- identity. Use the format origin-access-identity\/cloudfront\/Id where Id
-- is the value that CloudFront returned in the Id element when you created
-- the origin access identity.
socOriginAccessIdentity :: Lens' S3OriginConfig Text
socOriginAccessIdentity = lens _socOriginAccessIdentity (\ s a -> s{_socOriginAccessIdentity = a});

instance FromXML S3OriginConfig where
        parseXML x
          = S3OriginConfig' <$> (x .@ "OriginAccessIdentity")

instance ToXML S3OriginConfig where
        toXML S3OriginConfig'{..}
          = mconcat
              ["OriginAccessIdentity" @= _socOriginAccessIdentity]

-- | A complex type that lists the AWS accounts that were included in the
-- TrustedSigners complex type, as well as their active CloudFront key pair
-- IDs, if any.
--
-- /See:/ 'signer' smart constructor.
data Signer = Signer'
    { _sAWSAccountNumber :: !(Maybe Text)
    , _sKeyPairIds       :: !(Maybe KeyPairIds)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Signer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAWSAccountNumber'
--
-- * 'sKeyPairIds'
signer
    :: Signer
signer =
    Signer'
    { _sAWSAccountNumber = Nothing
    , _sKeyPairIds = Nothing
    }

-- | Specifies an AWS account that can create signed URLs. Values: self,
-- which indicates that the AWS account that was used to create the
-- distribution can created signed URLs, or an AWS account number. Omit the
-- dashes in the account number.
sAWSAccountNumber :: Lens' Signer (Maybe Text)
sAWSAccountNumber = lens _sAWSAccountNumber (\ s a -> s{_sAWSAccountNumber = a});

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
sKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sKeyPairIds = lens _sKeyPairIds (\ s a -> s{_sKeyPairIds = a});

instance FromXML Signer where
        parseXML x
          = Signer' <$>
              (x .@? "AwsAccountNumber") <*> (x .@? "KeyPairIds")

-- | A streaming distribution.
--
-- /See:/ 'streamingDistribution' smart constructor.
data StreamingDistribution = StreamingDistribution'
    { _sdLastModifiedTime            :: !(Maybe ISO8601)
    , _sdId                          :: !Text
    , _sdStatus                      :: !Text
    , _sdDomainName                  :: !Text
    , _sdActiveTrustedSigners        :: !ActiveTrustedSigners
    , _sdStreamingDistributionConfig :: !StreamingDistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLastModifiedTime'
--
-- * 'sdId'
--
-- * 'sdStatus'
--
-- * 'sdDomainName'
--
-- * 'sdActiveTrustedSigners'
--
-- * 'sdStreamingDistributionConfig'
streamingDistribution
    :: Text -- ^ 'sdId'
    -> Text -- ^ 'sdStatus'
    -> Text -- ^ 'sdDomainName'
    -> ActiveTrustedSigners -- ^ 'sdActiveTrustedSigners'
    -> StreamingDistributionConfig -- ^ 'sdStreamingDistributionConfig'
    -> StreamingDistribution
streamingDistribution pId_ pStatus_ pDomainName_ pActiveTrustedSigners_ pStreamingDistributionConfig_ =
    StreamingDistribution'
    { _sdLastModifiedTime = Nothing
    , _sdId = pId_
    , _sdStatus = pStatus_
    , _sdDomainName = pDomainName_
    , _sdActiveTrustedSigners = pActiveTrustedSigners_
    , _sdStreamingDistributionConfig = pStreamingDistributionConfig_
    }

-- | The date and time the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe UTCTime)
sdLastModifiedTime = lens _sdLastModifiedTime (\ s a -> s{_sdLastModifiedTime = a}) . mapping _Time;

-- | The identifier for the streaming distribution. For example:
-- EGTXBD79H29TRA8.
sdId :: Lens' StreamingDistribution Text
sdId = lens _sdId (\ s a -> s{_sdId = a});

-- | The current status of the streaming distribution. When the status is
-- Deployed, the distribution\'s information is fully propagated throughout
-- the Amazon CloudFront system.
sdStatus :: Lens' StreamingDistribution Text
sdStatus = lens _sdStatus (\ s a -> s{_sdStatus = a});

-- | The domain name corresponding to the streaming distribution. For
-- example: s5c39gqb8ow64r.cloudfront.net.
sdDomainName :: Lens' StreamingDistribution Text
sdDomainName = lens _sdDomainName (\ s a -> s{_sdDomainName = a});

-- | CloudFront automatically adds this element to the response only if
-- you\'ve set up the distribution to serve private content with signed
-- URLs. The element lists the key pair IDs that CloudFront is aware of for
-- each trusted signer. The Signer child element lists the AWS account
-- number of the trusted signer (or an empty Self element if the signer is
-- you). The Signer element also includes the IDs of any active key pairs
-- associated with the trusted signer\'s AWS account. If no KeyPairId
-- element appears for a Signer, that signer can\'t create working signed
-- URLs.
sdActiveTrustedSigners :: Lens' StreamingDistribution ActiveTrustedSigners
sdActiveTrustedSigners = lens _sdActiveTrustedSigners (\ s a -> s{_sdActiveTrustedSigners = a});

-- | The current configuration information for the streaming distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution StreamingDistributionConfig
sdStreamingDistributionConfig = lens _sdStreamingDistributionConfig (\ s a -> s{_sdStreamingDistributionConfig = a});

instance FromXML StreamingDistribution where
        parseXML x
          = StreamingDistribution' <$>
              (x .@? "LastModifiedTime") <*> (x .@ "Id") <*>
                (x .@ "Status")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "StreamingDistributionConfig")

-- | The configuration for the streaming distribution.
--
-- /See:/ 'streamingDistributionConfig' smart constructor.
data StreamingDistributionConfig = StreamingDistributionConfig'
    { _sdcAliases         :: !(Maybe Aliases)
    , _sdcPriceClass      :: !(Maybe PriceClass)
    , _sdcLogging         :: !(Maybe StreamingLoggingConfig)
    , _sdcCallerReference :: !Text
    , _sdcS3Origin        :: !S3Origin
    , _sdcComment         :: !Text
    , _sdcTrustedSigners  :: !TrustedSigners
    , _sdcEnabled         :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StreamingDistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcAliases'
--
-- * 'sdcPriceClass'
--
-- * 'sdcLogging'
--
-- * 'sdcCallerReference'
--
-- * 'sdcS3Origin'
--
-- * 'sdcComment'
--
-- * 'sdcTrustedSigners'
--
-- * 'sdcEnabled'
streamingDistributionConfig
    :: Text -- ^ 'sdcCallerReference'
    -> S3Origin -- ^ 'sdcS3Origin'
    -> Text -- ^ 'sdcComment'
    -> TrustedSigners -- ^ 'sdcTrustedSigners'
    -> Bool -- ^ 'sdcEnabled'
    -> StreamingDistributionConfig
streamingDistributionConfig pCallerReference_ pS3Origin_ pComment_ pTrustedSigners_ pEnabled_ =
    StreamingDistributionConfig'
    { _sdcAliases = Nothing
    , _sdcPriceClass = Nothing
    , _sdcLogging = Nothing
    , _sdcCallerReference = pCallerReference_
    , _sdcS3Origin = pS3Origin_
    , _sdcComment = pComment_
    , _sdcTrustedSigners = pTrustedSigners_
    , _sdcEnabled = pEnabled_
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Maybe Aliases)
sdcAliases = lens _sdcAliases (\ s a -> s{_sdcAliases = a});

-- | A complex type that contains information about price class for this
-- streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (Maybe PriceClass)
sdcPriceClass = lens _sdcPriceClass (\ s a -> s{_sdcPriceClass = a});

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (Maybe StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\ s a -> s{_sdcLogging = a});

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the
-- StreamingDistributionConfig object), a new streaming distribution is
-- created. If the CallerReference is a value you already sent in a
-- previous request to create a streaming distribution, and the content of
-- the StreamingDistributionConfig is identical to the original request
-- (ignoring white space), the response includes the same information
-- returned to the original request. If the CallerReference is a value you
-- already sent in a previous request to create a streaming distribution
-- but the content of the StreamingDistributionConfig is different from the
-- original request, CloudFront returns a DistributionAlreadyExists error.
sdcCallerReference :: Lens' StreamingDistributionConfig Text
sdcCallerReference = lens _sdcCallerReference (\ s a -> s{_sdcCallerReference = a});

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = lens _sdcS3Origin (\ s a -> s{_sdcS3Origin = a});

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig Text
sdcComment = lens _sdcComment (\ s a -> s{_sdcComment = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
sdcTrustedSigners :: Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners = lens _sdcTrustedSigners (\ s a -> s{_sdcTrustedSigners = a});

-- | Whether the streaming distribution is enabled to accept end user
-- requests for content.
sdcEnabled :: Lens' StreamingDistributionConfig Bool
sdcEnabled = lens _sdcEnabled (\ s a -> s{_sdcEnabled = a});

instance FromXML StreamingDistributionConfig where
        parseXML x
          = StreamingDistributionConfig' <$>
              (x .@? "Aliases") <*> (x .@? "PriceClass") <*>
                (x .@? "Logging")
                <*> (x .@ "CallerReference")
                <*> (x .@ "S3Origin")
                <*> (x .@ "Comment")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "Enabled")

instance ToXML StreamingDistributionConfig where
        toXML StreamingDistributionConfig'{..}
          = mconcat
              ["Aliases" @= _sdcAliases,
               "PriceClass" @= _sdcPriceClass,
               "Logging" @= _sdcLogging,
               "CallerReference" @= _sdcCallerReference,
               "S3Origin" @= _sdcS3Origin, "Comment" @= _sdcComment,
               "TrustedSigners" @= _sdcTrustedSigners,
               "Enabled" @= _sdcEnabled]

-- | A streaming distribution list.
--
-- /See:/ 'streamingDistributionList' smart constructor.
data StreamingDistributionList = StreamingDistributionList'
    { _sdlItems       :: !(Maybe [StreamingDistributionSummary])
    , _sdlNextMarker  :: !(Maybe Text)
    , _sdlMarker      :: !Text
    , _sdlMaxItems    :: !Int
    , _sdlIsTruncated :: !Bool
    , _sdlQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StreamingDistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdlItems'
--
-- * 'sdlNextMarker'
--
-- * 'sdlMarker'
--
-- * 'sdlMaxItems'
--
-- * 'sdlIsTruncated'
--
-- * 'sdlQuantity'
streamingDistributionList
    :: Text -- ^ 'sdlMarker'
    -> Int -- ^ 'sdlMaxItems'
    -> Bool -- ^ 'sdlIsTruncated'
    -> Int -- ^ 'sdlQuantity'
    -> StreamingDistributionList
streamingDistributionList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
    StreamingDistributionList'
    { _sdlItems = Nothing
    , _sdlNextMarker = Nothing
    , _sdlMarker = pMarker_
    , _sdlMaxItems = pMaxItems_
    , _sdlIsTruncated = pIsTruncated_
    , _sdlQuantity = pQuantity_
    }

-- | A complex type that contains one StreamingDistributionSummary element
-- for each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList [StreamingDistributionSummary]
sdlItems = lens _sdlItems (\ s a -> s{_sdlItems = a}) . _Default . _Coerce;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- streaming distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\ s a -> s{_sdlNextMarker = a});

-- | The value you provided for the Marker request parameter.
sdlMarker :: Lens' StreamingDistributionList Text
sdlMarker = lens _sdlMarker (\ s a -> s{_sdlMarker = a});

-- | The value you provided for the MaxItems request parameter.
sdlMaxItems :: Lens' StreamingDistributionList Int
sdlMaxItems = lens _sdlMaxItems (\ s a -> s{_sdlMaxItems = a});

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- distributions in the list.
sdlIsTruncated :: Lens' StreamingDistributionList Bool
sdlIsTruncated = lens _sdlIsTruncated (\ s a -> s{_sdlIsTruncated = a});

-- | The number of streaming distributions that were created by the current
-- AWS account.
sdlQuantity :: Lens' StreamingDistributionList Int
sdlQuantity = lens _sdlQuantity (\ s a -> s{_sdlQuantity = a});

instance FromXML StreamingDistributionList where
        parseXML x
          = StreamingDistributionList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "StreamingDistributionSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | A summary of the information for an Amazon CloudFront streaming
-- distribution.
--
-- /See:/ 'streamingDistributionSummary' smart constructor.
data StreamingDistributionSummary = StreamingDistributionSummary'
    { _sdsId               :: !Text
    , _sdsStatus           :: !Text
    , _sdsLastModifiedTime :: !ISO8601
    , _sdsDomainName       :: !Text
    , _sdsS3Origin         :: !S3Origin
    , _sdsAliases          :: !Aliases
    , _sdsTrustedSigners   :: !TrustedSigners
    , _sdsComment          :: !Text
    , _sdsPriceClass       :: !PriceClass
    , _sdsEnabled          :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StreamingDistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsId'
--
-- * 'sdsStatus'
--
-- * 'sdsLastModifiedTime'
--
-- * 'sdsDomainName'
--
-- * 'sdsS3Origin'
--
-- * 'sdsAliases'
--
-- * 'sdsTrustedSigners'
--
-- * 'sdsComment'
--
-- * 'sdsPriceClass'
--
-- * 'sdsEnabled'
streamingDistributionSummary
    :: Text -- ^ 'sdsId'
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
streamingDistributionSummary pId_ pStatus_ pLastModifiedTime_ pDomainName_ pS3Origin_ pAliases_ pTrustedSigners_ pComment_ pPriceClass_ pEnabled_ =
    StreamingDistributionSummary'
    { _sdsId = pId_
    , _sdsStatus = pStatus_
    , _sdsLastModifiedTime = _Time # pLastModifiedTime_
    , _sdsDomainName = pDomainName_
    , _sdsS3Origin = pS3Origin_
    , _sdsAliases = pAliases_
    , _sdsTrustedSigners = pTrustedSigners_
    , _sdsComment = pComment_
    , _sdsPriceClass = pPriceClass_
    , _sdsEnabled = pEnabled_
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
sdsId :: Lens' StreamingDistributionSummary Text
sdsId = lens _sdsId (\ s a -> s{_sdsId = a});

-- | Indicates the current status of the distribution. When the status is
-- Deployed, the distribution\'s information is fully propagated throughout
-- the Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary Text
sdsStatus = lens _sdsStatus (\ s a -> s{_sdsStatus = a});

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary UTCTime
sdsLastModifiedTime = lens _sdsLastModifiedTime (\ s a -> s{_sdsLastModifiedTime = a}) . _Time;

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
sdsDomainName :: Lens' StreamingDistributionSummary Text
sdsDomainName = lens _sdsDomainName (\ s a -> s{_sdsDomainName = a});

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = lens _sdsS3Origin (\ s a -> s{_sdsS3Origin = a});

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary Aliases
sdsAliases = lens _sdsAliases (\ s a -> s{_sdsAliases = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
sdsTrustedSigners :: Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners = lens _sdsTrustedSigners (\ s a -> s{_sdsTrustedSigners = a});

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary Text
sdsComment = lens _sdsComment (\ s a -> s{_sdsComment = a});

-- | Undocumented member.
sdsPriceClass :: Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = lens _sdsPriceClass (\ s a -> s{_sdsPriceClass = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
sdsEnabled :: Lens' StreamingDistributionSummary Bool
sdsEnabled = lens _sdsEnabled (\ s a -> s{_sdsEnabled = a});

instance FromXML StreamingDistributionSummary where
        parseXML x
          = StreamingDistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "DomainName")
                <*> (x .@ "S3Origin")
                <*> (x .@ "Aliases")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "Comment")
                <*> (x .@ "PriceClass")
                <*> (x .@ "Enabled")

-- | A complex type that controls whether access logs are written for this
-- streaming distribution.
--
-- /See:/ 'streamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
    { _slcEnabled :: !Bool
    , _slcBucket  :: !Text
    , _slcPrefix  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StreamingLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEnabled'
--
-- * 'slcBucket'
--
-- * 'slcPrefix'
streamingLoggingConfig
    :: Bool -- ^ 'slcEnabled'
    -> Text -- ^ 'slcBucket'
    -> Text -- ^ 'slcPrefix'
    -> StreamingLoggingConfig
streamingLoggingConfig pEnabled_ pBucket_ pPrefix_ =
    StreamingLoggingConfig'
    { _slcEnabled = pEnabled_
    , _slcBucket = pBucket_
    , _slcPrefix = pPrefix_
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you do not want to enable logging when you create a
-- streaming distribution or if you want to disable logging for an existing
-- streaming distribution, specify false for Enabled, and specify empty
-- Bucket and Prefix elements. If you specify false for Enabled but you
-- specify values for Bucket and Prefix, the values are automatically
-- deleted.
slcEnabled :: Lens' StreamingLoggingConfig Bool
slcEnabled = lens _slcEnabled (\ s a -> s{_slcEnabled = a});

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
slcBucket :: Lens' StreamingLoggingConfig Text
slcBucket = lens _slcBucket (\ s a -> s{_slcBucket = a});

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, myprefix\/. If
-- you want to enable logging, but you do not want to specify a prefix, you
-- still must include an empty Prefix element in the Logging element.
slcPrefix :: Lens' StreamingLoggingConfig Text
slcPrefix = lens _slcPrefix (\ s a -> s{_slcPrefix = a});

instance FromXML StreamingLoggingConfig where
        parseXML x
          = StreamingLoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "Bucket") <*>
                (x .@ "Prefix")

instance ToXML StreamingLoggingConfig where
        toXML StreamingLoggingConfig'{..}
          = mconcat
              ["Enabled" @= _slcEnabled, "Bucket" @= _slcBucket,
               "Prefix" @= _slcPrefix]

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
--
-- /See:/ 'trustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
    { _tsItems    :: !(Maybe [Text])
    , _tsEnabled  :: !Bool
    , _tsQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsItems'
--
-- * 'tsEnabled'
--
-- * 'tsQuantity'
trustedSigners
    :: Bool -- ^ 'tsEnabled'
    -> Int -- ^ 'tsQuantity'
    -> TrustedSigners
trustedSigners pEnabled_ pQuantity_ =
    TrustedSigners'
    { _tsItems = Nothing
    , _tsEnabled = pEnabled_
    , _tsQuantity = pQuantity_
    }

-- | Optional: A complex type that contains trusted signers for this cache
-- behavior. If Quantity is 0, you can omit Items.
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\ s a -> s{_tsItems = a}) . _Default . _Coerce;

-- | Specifies whether you want to require end users to use signed URLs to
-- access the files specified by PathPattern and TargetOriginId.
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\ s a -> s{_tsEnabled = a});

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\ s a -> s{_tsQuantity = a});

instance FromXML TrustedSigners where
        parseXML x
          = TrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "AwsAccountNumber"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance ToXML TrustedSigners where
        toXML TrustedSigners'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "AwsAccountNumber" <$> _tsItems),
               "Enabled" @= _tsEnabled, "Quantity" @= _tsQuantity]

-- | A complex type that contains information about viewer certificates for
-- this distribution.
--
-- /See:/ 'viewerCertificate' smart constructor.
data ViewerCertificate = ViewerCertificate'
    { _vcSSLSupportMethod             :: !(Maybe SSLSupportMethod)
    , _vcMinimumProtocolVersion       :: !(Maybe MinimumProtocolVersion)
    , _vcIAMCertificateId             :: !(Maybe Text)
    , _vcCloudFrontDefaultCertificate :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ViewerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSSLSupportMethod'
--
-- * 'vcMinimumProtocolVersion'
--
-- * 'vcIAMCertificateId'
--
-- * 'vcCloudFrontDefaultCertificate'
viewerCertificate
    :: ViewerCertificate
viewerCertificate =
    ViewerCertificate'
    { _vcSSLSupportMethod = Nothing
    , _vcMinimumProtocolVersion = Nothing
    , _vcIAMCertificateId = Nothing
    , _vcCloudFrontDefaultCertificate = Nothing
    }

-- | If you specify a value for IAMCertificateId, you must also specify how
-- you want CloudFront to serve HTTPS requests. Valid values are vip and
-- sni-only. If you specify vip, CloudFront uses dedicated IP addresses for
-- your content and can respond to HTTPS requests from any viewer. However,
-- you must request permission to use this feature, and you incur
-- additional monthly charges. If you specify sni-only, CloudFront can only
-- respond to HTTPS requests from viewers that support Server Name
-- Indication (SNI). All modern browsers support SNI, but some browsers
-- still in use don\'t support SNI. Do not specify a value for
-- SSLSupportMethod if you specified true for CloudFrontDefaultCertificate.
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod = lens _vcSSLSupportMethod (\ s a -> s{_vcSSLSupportMethod = a});

-- | Specify the minimum version of the SSL protocol that you want CloudFront
-- to use, SSLv3 or TLSv1, for HTTPS connections. CloudFront will serve
-- your objects only to browsers or devices that support at least the SSL
-- version that you specify. The TLSv1 protocol is more secure, so we
-- recommend that you specify SSLv3 only if your users are using browsers
-- or devices that don\'t support TLSv1. If you\'re using a custom
-- certificate (if you specify a value for IAMCertificateId) and if you\'re
-- using dedicated IP (if you specify vip for SSLSupportMethod), you can
-- choose SSLv3 or TLSv1 as the MinimumProtocolVersion. If you\'re using a
-- custom certificate (if you specify a value for IAMCertificateId) and if
-- you\'re using SNI (if you specify sni-only for SSLSupportMethod), you
-- must specify TLSv1 for MinimumProtocolVersion.
vcMinimumProtocolVersion :: Lens' ViewerCertificate (Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion = lens _vcMinimumProtocolVersion (\ s a -> s{_vcMinimumProtocolVersion = a});

-- | If you want viewers to use HTTPS to request your objects and you\'re
-- using an alternate domain name in your object URLs (for example,
-- https:\/\/example.com\/logo.jpg), specify the IAM certificate identifier
-- of the custom viewer certificate for this distribution. Specify either
-- this value or CloudFrontDefaultCertificate.
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId = lens _vcIAMCertificateId (\ s a -> s{_vcIAMCertificateId = a});

-- | If you want viewers to use HTTPS to request your objects and you\'re
-- using the CloudFront domain name of your distribution in your object
-- URLs (for example, https:\/\/d111111abcdef8.cloudfront.net\/logo.jpg),
-- set to true. Omit this value if you are setting an IAMCertificateId.
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate = lens _vcCloudFrontDefaultCertificate (\ s a -> s{_vcCloudFrontDefaultCertificate = a});

instance FromXML ViewerCertificate where
        parseXML x
          = ViewerCertificate' <$>
              (x .@? "SSLSupportMethod") <*>
                (x .@? "MinimumProtocolVersion")
                <*> (x .@? "IAMCertificateId")
                <*> (x .@? "CloudFrontDefaultCertificate")

instance ToXML ViewerCertificate where
        toXML ViewerCertificate'{..}
          = mconcat
              ["SSLSupportMethod" @= _vcSSLSupportMethod,
               "MinimumProtocolVersion" @=
                 _vcMinimumProtocolVersion,
               "IAMCertificateId" @= _vcIAMCertificateId,
               "CloudFrontDefaultCertificate" @=
                 _vcCloudFrontDefaultCertificate]
