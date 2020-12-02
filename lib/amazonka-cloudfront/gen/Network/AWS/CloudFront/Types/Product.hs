{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Product where

import Network.AWS.CloudFront.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content.
--
--
-- The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs.
--
-- For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'activeTrustedSigners' smart constructor.
data ActiveTrustedSigners = ActiveTrustedSigners'
  { _atsItems    :: !(Maybe [Signer])
  , _atsEnabled  :: !Bool
  , _atsQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ActiveTrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atsItems' - A complex type that contains one @Signer@ complex type for each trusted signer that is specified in the @TrustedSigners@ complex type. For more information, see 'ActiveTrustedSigners' .
--
-- * 'atsEnabled' - Enabled is @true@ if any of the AWS accounts listed in the @TrustedSigners@ complex type for this RTMP distribution have active CloudFront key pairs. If not, @Enabled@ is @false@ . For more information, see 'ActiveTrustedSigners' .
--
-- * 'atsQuantity' - A complex type that contains one @Signer@ complex type for each trusted signer specified in the @TrustedSigners@ complex type. For more information, see 'ActiveTrustedSigners' .
activeTrustedSigners
    :: Bool -- ^ 'atsEnabled'
    -> Int -- ^ 'atsQuantity'
    -> ActiveTrustedSigners
activeTrustedSigners pEnabled_ pQuantity_ =
  ActiveTrustedSigners'
    {_atsItems = Nothing, _atsEnabled = pEnabled_, _atsQuantity = pQuantity_}


-- | A complex type that contains one @Signer@ complex type for each trusted signer that is specified in the @TrustedSigners@ complex type. For more information, see 'ActiveTrustedSigners' .
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\ s a -> s{_atsItems = a}) . _Default . _Coerce

-- | Enabled is @true@ if any of the AWS accounts listed in the @TrustedSigners@ complex type for this RTMP distribution have active CloudFront key pairs. If not, @Enabled@ is @false@ . For more information, see 'ActiveTrustedSigners' .
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\ s a -> s{_atsEnabled = a})

-- | A complex type that contains one @Signer@ complex type for each trusted signer specified in the @TrustedSigners@ complex type. For more information, see 'ActiveTrustedSigners' .
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\ s a -> s{_atsQuantity = a})

instance FromXML ActiveTrustedSigners where
        parseXML x
          = ActiveTrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Signer"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance Hashable ActiveTrustedSigners where

instance NFData ActiveTrustedSigners where

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
--
--
-- /See:/ 'aliases' smart constructor.
data Aliases = Aliases'
  { _aItems    :: !(Maybe [Text])
  , _aQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Aliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aItems' - A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
--
-- * 'aQuantity' - The number of CNAME aliases, if any, that you want to associate with this distribution.
aliases
    :: Int -- ^ 'aQuantity'
    -> Aliases
aliases pQuantity_ = Aliases' {_aItems = Nothing, _aQuantity = pQuantity_}


-- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
aItems :: Lens' Aliases [Text]
aItems = lens _aItems (\ s a -> s{_aItems = a}) . _Default . _Coerce

-- | The number of CNAME aliases, if any, that you want to associate with this distribution.
aQuantity :: Lens' Aliases Int
aQuantity = lens _aQuantity (\ s a -> s{_aQuantity = a})

instance FromXML Aliases where
        parseXML x
          = Aliases' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CNAME"))
                <*> (x .@ "Quantity")

instance Hashable Aliases where

instance NFData Aliases where

instance ToXML Aliases where
        toXML Aliases'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "CNAME" <$> _aItems),
               "Quantity" @= _aQuantity]

-- | A complex type that controls which HTTP methods CloudFront processes and forwards to your Amazon S3 bucket or your custom origin. There are three choices:
--
--
--     * CloudFront forwards only @GET@ and @HEAD@ requests.
--
--     * CloudFront forwards only @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--     * CloudFront forwards @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests.
--
--
--
-- If you pick the third choice, you may need to restrict access to your Amazon S3 bucket or to your custom origin so users can't perform operations that you don't want them to. For example, you might not want users to have permissions to delete objects from your origin.
--
--
-- /See:/ 'allowedMethods' smart constructor.
data AllowedMethods = AllowedMethods'
  { _amCachedMethods :: !(Maybe CachedMethods)
  , _amQuantity      :: !Int
  , _amItems         :: ![Method]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllowedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amCachedMethods' - Undocumented member.
--
-- * 'amQuantity' - The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
--
-- * 'amItems' - A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
allowedMethods
    :: Int -- ^ 'amQuantity'
    -> AllowedMethods
allowedMethods pQuantity_ =
  AllowedMethods'
    {_amCachedMethods = Nothing, _amQuantity = pQuantity_, _amItems = mempty}


-- | Undocumented member.
amCachedMethods :: Lens' AllowedMethods (Maybe CachedMethods)
amCachedMethods = lens _amCachedMethods (\ s a -> s{_amCachedMethods = a})

-- | The number of HTTP methods that you want CloudFront to forward to your origin. Valid values are 2 (for @GET@ and @HEAD@ requests), 3 (for @GET@ , @HEAD@ , and @OPTIONS@ requests) and 7 (for @GET, HEAD, OPTIONS, PUT, PATCH, POST@ , and @DELETE@ requests).
amQuantity :: Lens' AllowedMethods Int
amQuantity = lens _amQuantity (\ s a -> s{_amQuantity = a})

-- | A complex type that contains the HTTP methods that you want CloudFront to process and forward to your origin.
amItems :: Lens' AllowedMethods [Method]
amItems = lens _amItems (\ s a -> s{_amItems = a}) . _Coerce

instance FromXML AllowedMethods where
        parseXML x
          = AllowedMethods' <$>
              (x .@? "CachedMethods") <*> (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance Hashable AllowedMethods where

instance NFData AllowedMethods where

instance ToXML AllowedMethods where
        toXML AllowedMethods'{..}
          = mconcat
              ["CachedMethods" @= _amCachedMethods,
               "Quantity" @= _amQuantity,
               "Items" @= toXMLList "Method" _amItems]

-- | A complex type that describes how CloudFront processes requests.
--
--
-- You must create at least as many cache behaviors (including the default cache behavior) as you have origins if you want CloudFront to distribute objects from all of the origins. Each cache behavior specifies the one origin from which you want CloudFront to get objects. If you have two origins and only the default cache behavior, the default cache behavior will cause CloudFront to get objects from one of the origins, but the other origin is never used.
--
-- For the current limit on the number of cache behaviors that you can add to a distribution, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
--
-- If you don't want to specify any cache behaviors, include only an empty @CacheBehaviors@ element. Don't include an empty @CacheBehavior@ element, or CloudFront returns a @MalformedXML@ error.
--
-- To delete all cache behaviors in an existing distribution, update the distribution configuration and include only an empty @CacheBehaviors@ element.
--
-- To add, change, or remove one or more cache behaviors, update the distribution configuration and specify all of the cache behaviors that you want to include in the updated distribution.
--
-- For more information about cache behaviors, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behaviors> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'cacheBehavior' smart constructor.
data CacheBehavior = CacheBehavior'
  { _cbAllowedMethods             :: !(Maybe AllowedMethods)
  , _cbLambdaFunctionAssociations :: !(Maybe LambdaFunctionAssociations)
  , _cbMaxTTL                     :: !(Maybe Integer)
  , _cbCompress                   :: !(Maybe Bool)
  , _cbSmoothStreaming            :: !(Maybe Bool)
  , _cbDefaultTTL                 :: !(Maybe Integer)
  , _cbFieldLevelEncryptionId     :: !(Maybe Text)
  , _cbPathPattern                :: !Text
  , _cbTargetOriginId             :: !Text
  , _cbForwardedValues            :: !ForwardedValues
  , _cbTrustedSigners             :: !TrustedSigners
  , _cbViewerProtocolPolicy       :: !ViewerProtocolPolicy
  , _cbMinTTL                     :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbAllowedMethods' - Undocumented member.
--
-- * 'cbLambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- * 'cbMaxTTL' - The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbCompress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbSmoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- * 'cbDefaultTTL' - The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbFieldLevelEncryptionId' - Undocumented member.
--
-- * 'cbPathPattern' - The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbTargetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior.
--
-- * 'cbForwardedValues' - A complex type that specifies how CloudFront handles query strings and cookies.
--
-- * 'cbTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- * 'cbViewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'cbMinTTL' - The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
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
    , _cbLambdaFunctionAssociations = Nothing
    , _cbMaxTTL = Nothing
    , _cbCompress = Nothing
    , _cbSmoothStreaming = Nothing
    , _cbDefaultTTL = Nothing
    , _cbFieldLevelEncryptionId = Nothing
    , _cbPathPattern = pPathPattern_
    , _cbTargetOriginId = pTargetOriginId_
    , _cbForwardedValues = pForwardedValues_
    , _cbTrustedSigners = pTrustedSigners_
    , _cbViewerProtocolPolicy = pViewerProtocolPolicy_
    , _cbMinTTL = pMinTTL_
    }


-- | Undocumented member.
cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\ s a -> s{_cbAllowedMethods = a})

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
cbLambdaFunctionAssociations :: Lens' CacheBehavior (Maybe LambdaFunctionAssociations)
cbLambdaFunctionAssociations = lens _cbLambdaFunctionAssociations (\ s a -> s{_cbLambdaFunctionAssociations = a})

-- | The maximum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin adds HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbMaxTTL :: Lens' CacheBehavior (Maybe Integer)
cbMaxTTL = lens _cbMaxTTL (\ s a -> s{_cbMaxTTL = a})

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify true; if not, specify false. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
cbCompress :: Lens' CacheBehavior (Maybe Bool)
cbCompress = lens _cbCompress (\ s a -> s{_cbCompress = a})

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming = lens _cbSmoothStreaming (\ s a -> s{_cbSmoothStreaming = a})

-- | The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
cbDefaultTTL :: Lens' CacheBehavior (Maybe Integer)
cbDefaultTTL = lens _cbDefaultTTL (\ s a -> s{_cbDefaultTTL = a})

-- | Undocumented member.
cbFieldLevelEncryptionId :: Lens' CacheBehavior (Maybe Text)
cbFieldLevelEncryptionId = lens _cbFieldLevelEncryptionId (\ s a -> s{_cbFieldLevelEncryptionId = a})

-- | The pattern (for example, @images/*.jpg@ ) that specifies which requests to apply the behavior to. When CloudFront receives a viewer request, the requested path is compared with path patterns in the order in which cache behaviors are listed in the distribution. The path pattern for the default cache behavior is @*@ and cannot be changed. If the request for an object does not match the path pattern for any cache behaviors, CloudFront applies the behavior in the default cache behavior. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesPathPattern Path Pattern> in the /Amazon CloudFront Developer Guide/ .
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\ s a -> s{_cbPathPattern = a})

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\ s a -> s{_cbTargetOriginId = a})

-- | A complex type that specifies how CloudFront handles query strings and cookies.
cbForwardedValues :: Lens' CacheBehavior ForwardedValues
cbForwardedValues = lens _cbForwardedValues (\ s a -> s{_cbForwardedValues = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
cbTrustedSigners :: Lens' CacheBehavior TrustedSigners
cbTrustedSigners = lens _cbTrustedSigners (\ s a -> s{_cbTrustedSigners = a})

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.      * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden).  For more information about requiring the HTTPS protocol, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = lens _cbViewerProtocolPolicy (\ s a -> s{_cbViewerProtocolPolicy = a})

-- | The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
cbMinTTL :: Lens' CacheBehavior Integer
cbMinTTL = lens _cbMinTTL (\ s a -> s{_cbMinTTL = a})

instance FromXML CacheBehavior where
        parseXML x
          = CacheBehavior' <$>
              (x .@? "AllowedMethods") <*>
                (x .@? "LambdaFunctionAssociations")
                <*> (x .@? "MaxTTL")
                <*> (x .@? "Compress")
                <*> (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@? "FieldLevelEncryptionId")
                <*> (x .@ "PathPattern")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance Hashable CacheBehavior where

instance NFData CacheBehavior where

instance ToXML CacheBehavior where
        toXML CacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _cbAllowedMethods,
               "LambdaFunctionAssociations" @=
                 _cbLambdaFunctionAssociations,
               "MaxTTL" @= _cbMaxTTL, "Compress" @= _cbCompress,
               "SmoothStreaming" @= _cbSmoothStreaming,
               "DefaultTTL" @= _cbDefaultTTL,
               "FieldLevelEncryptionId" @=
                 _cbFieldLevelEncryptionId,
               "PathPattern" @= _cbPathPattern,
               "TargetOriginId" @= _cbTargetOriginId,
               "ForwardedValues" @= _cbForwardedValues,
               "TrustedSigners" @= _cbTrustedSigners,
               "ViewerProtocolPolicy" @= _cbViewerProtocolPolicy,
               "MinTTL" @= _cbMinTTL]

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
--
--
-- /See:/ 'cacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { _cbItems    :: !(Maybe [CacheBehavior])
  , _cbQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbItems' - Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'cbQuantity' - The number of cache behaviors for this distribution.
cacheBehaviors
    :: Int -- ^ 'cbQuantity'
    -> CacheBehaviors
cacheBehaviors pQuantity_ =
  CacheBehaviors' {_cbItems = Nothing, _cbQuantity = pQuantity_}


-- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
cbItems :: Lens' CacheBehaviors [CacheBehavior]
cbItems = lens _cbItems (\ s a -> s{_cbItems = a}) . _Default . _Coerce

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors Int
cbQuantity = lens _cbQuantity (\ s a -> s{_cbQuantity = a})

instance FromXML CacheBehaviors where
        parseXML x
          = CacheBehaviors' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CacheBehavior"))
                <*> (x .@ "Quantity")

instance Hashable CacheBehaviors where

instance NFData CacheBehaviors where

instance ToXML CacheBehaviors where
        toXML CacheBehaviors'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "CacheBehavior" <$> _cbItems),
               "Quantity" @= _cbQuantity]

-- | A complex type that controls whether CloudFront caches the response to requests using the specified HTTP methods. There are two choices:
--
--
--     * CloudFront caches responses to @GET@ and @HEAD@ requests.
--
--     * CloudFront caches responses to @GET@ , @HEAD@ , and @OPTIONS@ requests.
--
--
--
-- If you pick the second choice for your Amazon S3 Origin, you may need to forward Access-Control-Request-Method, Access-Control-Request-Headers, and Origin headers for the responses to be cached correctly.
--
--
-- /See:/ 'cachedMethods' smart constructor.
data CachedMethods = CachedMethods'
  { _cmQuantity :: !Int
  , _cmItems    :: ![Method]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CachedMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmQuantity' - The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
--
-- * 'cmItems' - A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
cachedMethods
    :: Int -- ^ 'cmQuantity'
    -> CachedMethods
cachedMethods pQuantity_ =
  CachedMethods' {_cmQuantity = pQuantity_, _cmItems = mempty}


-- | The number of HTTP methods for which you want CloudFront to cache responses. Valid values are @2@ (for caching responses to @GET@ and @HEAD@ requests) and @3@ (for caching responses to @GET@ , @HEAD@ , and @OPTIONS@ requests).
cmQuantity :: Lens' CachedMethods Int
cmQuantity = lens _cmQuantity (\ s a -> s{_cmQuantity = a})

-- | A complex type that contains the HTTP methods that you want CloudFront to cache responses to.
cmItems :: Lens' CachedMethods [Method]
cmItems = lens _cmItems (\ s a -> s{_cmItems = a}) . _Coerce

instance FromXML CachedMethods where
        parseXML x
          = CachedMethods' <$>
              (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance Hashable CachedMethods where

instance NFData CachedMethods where

instance ToXML CachedMethods where
        toXML CachedMethods'{..}
          = mconcat
              ["Quantity" @= _cmQuantity,
               "Items" @= toXMLList "Method" _cmItems]

-- | CloudFront origin access identity.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
  { _cfoaiCloudFrontOriginAccessIdentityConfig :: !(Maybe CloudFrontOriginAccessIdentityConfig)
  , _cfoaiId :: !Text
  , _cfoaiS3CanonicalUserId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaiCloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
--
-- * 'cfoaiId' - The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
--
-- * 'cfoaiS3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
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
cfoaiCloudFrontOriginAccessIdentityConfig = lens _cfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_cfoaiCloudFrontOriginAccessIdentityConfig = a})

-- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
cfoaiId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiId = lens _cfoaiId (\ s a -> s{_cfoaiId = a})

-- | The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiS3CanonicalUserId = lens _cfoaiS3CanonicalUserId (\ s a -> s{_cfoaiS3CanonicalUserId = a})

instance FromXML CloudFrontOriginAccessIdentity where
        parseXML x
          = CloudFrontOriginAccessIdentity' <$>
              (x .@? "CloudFrontOriginAccessIdentityConfig") <*>
                (x .@ "Id")
                <*> (x .@ "S3CanonicalUserId")

instance Hashable CloudFrontOriginAccessIdentity
         where

instance NFData CloudFrontOriginAccessIdentity where

-- | Origin access identity configuration. Send a @GET@ request to the @//CloudFront API version/ /CloudFront/identity ID/config@ resource.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
  { _cfoaicCallerReference :: !Text
  , _cfoaicComment         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaicCallerReference' - A unique number that ensures the request can't be replayed. If the @CallerReference@ is new (no matter the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created. If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.  If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
--
-- * 'cfoaicComment' - Any comments you want to include about the origin access identity.
cloudFrontOriginAccessIdentityConfig
    :: Text -- ^ 'cfoaicCallerReference'
    -> Text -- ^ 'cfoaicComment'
    -> CloudFrontOriginAccessIdentityConfig
cloudFrontOriginAccessIdentityConfig pCallerReference_ pComment_ =
  CloudFrontOriginAccessIdentityConfig'
    {_cfoaicCallerReference = pCallerReference_, _cfoaicComment = pComment_}


-- | A unique number that ensures the request can't be replayed. If the @CallerReference@ is new (no matter the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created. If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.  If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicCallerReference = lens _cfoaicCallerReference (\ s a -> s{_cfoaicCallerReference = a})

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicComment = lens _cfoaicComment (\ s a -> s{_cfoaicComment = a})

instance FromXML CloudFrontOriginAccessIdentityConfig
         where
        parseXML x
          = CloudFrontOriginAccessIdentityConfig' <$>
              (x .@ "CallerReference") <*> (x .@ "Comment")

instance Hashable
           CloudFrontOriginAccessIdentityConfig
         where

instance NFData CloudFrontOriginAccessIdentityConfig
         where

instance ToXML CloudFrontOriginAccessIdentityConfig
         where
        toXML CloudFrontOriginAccessIdentityConfig'{..}
          = mconcat
              ["CallerReference" @= _cfoaicCallerReference,
               "Comment" @= _cfoaicComment]

-- | Lists the origin access identities for CloudFront.Send a @GET@ request to the @//CloudFront API version/ /origin-access-identity/cloudfront@ resource. The response includes a @CloudFrontOriginAccessIdentityList@ element with zero or more @CloudFrontOriginAccessIdentitySummary@ child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the @MaxItems@ and @Marker@ parameters.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
  { _cfoailItems       :: !(Maybe [CloudFrontOriginAccessIdentitySummary])
  , _cfoailNextMarker  :: !(Maybe Text)
  , _cfoailMarker      :: !Text
  , _cfoailMaxItems    :: !Int
  , _cfoailIsTruncated :: !Bool
  , _cfoailQuantity    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFrontOriginAccessIdentityList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoailItems' - A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
--
-- * 'cfoailNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
--
-- * 'cfoailMarker' - Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
--
-- * 'cfoailMaxItems' - The maximum number of origin access identities you want in the response body.
--
-- * 'cfoailIsTruncated' - A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
--
-- * 'cfoailQuantity' - The number of CloudFront origin access identities that were created by the current AWS account.
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


-- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList [CloudFrontOriginAccessIdentitySummary]
cfoailItems = lens _cfoailItems (\ s a -> s{_cfoailItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\ s a -> s{_cfoailNextMarker = a})

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList Text
cfoailMarker = lens _cfoailMarker (\ s a -> s{_cfoailMarker = a})

-- | The maximum number of origin access identities you want in the response body.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailMaxItems = lens _cfoailMaxItems (\ s a -> s{_cfoailMaxItems = a})

-- | A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList Bool
cfoailIsTruncated = lens _cfoailIsTruncated (\ s a -> s{_cfoailIsTruncated = a})

-- | The number of CloudFront origin access identities that were created by the current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailQuantity = lens _cfoailQuantity (\ s a -> s{_cfoailQuantity = a})

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

instance Hashable CloudFrontOriginAccessIdentityList
         where

instance NFData CloudFrontOriginAccessIdentityList
         where

-- | Summary of the information about a CloudFront origin access identity.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
  { _cfoaisId                :: !Text
  , _cfoaisS3CanonicalUserId :: !Text
  , _cfoaisComment           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudFrontOriginAccessIdentitySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoaisId' - The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
--
-- * 'cfoaisS3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
--
-- * 'cfoaisComment' - The comment for this origin access identity, as originally specified when created.
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


-- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisId = lens _cfoaisId (\ s a -> s{_cfoaisId = a})

-- | The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisS3CanonicalUserId = lens _cfoaisS3CanonicalUserId (\ s a -> s{_cfoaisS3CanonicalUserId = a})

-- | The comment for this origin access identity, as originally specified when created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisComment = lens _cfoaisComment (\ s a -> s{_cfoaisComment = a})

instance FromXML
           CloudFrontOriginAccessIdentitySummary
         where
        parseXML x
          = CloudFrontOriginAccessIdentitySummary' <$>
              (x .@ "Id") <*> (x .@ "S3CanonicalUserId") <*>
                (x .@ "Comment")

instance Hashable
           CloudFrontOriginAccessIdentitySummary
         where

instance NFData CloudFrontOriginAccessIdentitySummary
         where

-- | A field-level encryption content type profile.
--
--
--
-- /See:/ 'contentTypeProfile' smart constructor.
data ContentTypeProfile = ContentTypeProfile'
  { _ctpProfileId   :: !(Maybe Text)
  , _ctpFormat      :: !Format
  , _ctpContentType :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContentTypeProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpProfileId' - The profile ID for a field-level encryption content type-profile mapping.
--
-- * 'ctpFormat' - The format for a field-level encryption content type-profile mapping.
--
-- * 'ctpContentType' - The content type for a field-level encryption content type-profile mapping.
contentTypeProfile
    :: Format -- ^ 'ctpFormat'
    -> Text -- ^ 'ctpContentType'
    -> ContentTypeProfile
contentTypeProfile pFormat_ pContentType_ =
  ContentTypeProfile'
    { _ctpProfileId = Nothing
    , _ctpFormat = pFormat_
    , _ctpContentType = pContentType_
    }


-- | The profile ID for a field-level encryption content type-profile mapping.
ctpProfileId :: Lens' ContentTypeProfile (Maybe Text)
ctpProfileId = lens _ctpProfileId (\ s a -> s{_ctpProfileId = a})

-- | The format for a field-level encryption content type-profile mapping.
ctpFormat :: Lens' ContentTypeProfile Format
ctpFormat = lens _ctpFormat (\ s a -> s{_ctpFormat = a})

-- | The content type for a field-level encryption content type-profile mapping.
ctpContentType :: Lens' ContentTypeProfile Text
ctpContentType = lens _ctpContentType (\ s a -> s{_ctpContentType = a})

instance FromXML ContentTypeProfile where
        parseXML x
          = ContentTypeProfile' <$>
              (x .@? "ProfileId") <*> (x .@ "Format") <*>
                (x .@ "ContentType")

instance Hashable ContentTypeProfile where

instance NFData ContentTypeProfile where

instance ToXML ContentTypeProfile where
        toXML ContentTypeProfile'{..}
          = mconcat
              ["ProfileId" @= _ctpProfileId,
               "Format" @= _ctpFormat,
               "ContentType" @= _ctpContentType]

-- | The configuration for a field-level encryption content type-profile mapping.
--
--
--
-- /See:/ 'contentTypeProfileConfig' smart constructor.
data ContentTypeProfileConfig = ContentTypeProfileConfig'
  { _ctpcContentTypeProfiles             :: !(Maybe ContentTypeProfiles)
  , _ctpcForwardWhenContentTypeIsUnknown :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContentTypeProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpcContentTypeProfiles' - The configuration for a field-level encryption content type-profile.
--
-- * 'ctpcForwardWhenContentTypeIsUnknown' - The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
contentTypeProfileConfig
    :: Bool -- ^ 'ctpcForwardWhenContentTypeIsUnknown'
    -> ContentTypeProfileConfig
contentTypeProfileConfig pForwardWhenContentTypeIsUnknown_ =
  ContentTypeProfileConfig'
    { _ctpcContentTypeProfiles = Nothing
    , _ctpcForwardWhenContentTypeIsUnknown = pForwardWhenContentTypeIsUnknown_
    }


-- | The configuration for a field-level encryption content type-profile.
ctpcContentTypeProfiles :: Lens' ContentTypeProfileConfig (Maybe ContentTypeProfiles)
ctpcContentTypeProfiles = lens _ctpcContentTypeProfiles (\ s a -> s{_ctpcContentTypeProfiles = a})

-- | The setting in a field-level encryption content type-profile mapping that specifies what to do when an unknown content type is provided for the profile. If true, content is forwarded without being encrypted when the content type is unknown. If false (the default), an error is returned when the content type is unknown.
ctpcForwardWhenContentTypeIsUnknown :: Lens' ContentTypeProfileConfig Bool
ctpcForwardWhenContentTypeIsUnknown = lens _ctpcForwardWhenContentTypeIsUnknown (\ s a -> s{_ctpcForwardWhenContentTypeIsUnknown = a})

instance FromXML ContentTypeProfileConfig where
        parseXML x
          = ContentTypeProfileConfig' <$>
              (x .@? "ContentTypeProfiles") <*>
                (x .@ "ForwardWhenContentTypeIsUnknown")

instance Hashable ContentTypeProfileConfig where

instance NFData ContentTypeProfileConfig where

instance ToXML ContentTypeProfileConfig where
        toXML ContentTypeProfileConfig'{..}
          = mconcat
              ["ContentTypeProfiles" @= _ctpcContentTypeProfiles,
               "ForwardWhenContentTypeIsUnknown" @=
                 _ctpcForwardWhenContentTypeIsUnknown]

-- | Field-level encryption content type-profile.
--
--
--
-- /See:/ 'contentTypeProfiles' smart constructor.
data ContentTypeProfiles = ContentTypeProfiles'
  { _ctpItems    :: !(Maybe [ContentTypeProfile])
  , _ctpQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContentTypeProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpItems' - Items in a field-level encryption content type-profile mapping.
--
-- * 'ctpQuantity' - The number of field-level encryption content type-profile mappings.
contentTypeProfiles
    :: Int -- ^ 'ctpQuantity'
    -> ContentTypeProfiles
contentTypeProfiles pQuantity_ =
  ContentTypeProfiles' {_ctpItems = Nothing, _ctpQuantity = pQuantity_}


-- | Items in a field-level encryption content type-profile mapping.
ctpItems :: Lens' ContentTypeProfiles [ContentTypeProfile]
ctpItems = lens _ctpItems (\ s a -> s{_ctpItems = a}) . _Default . _Coerce

-- | The number of field-level encryption content type-profile mappings.
ctpQuantity :: Lens' ContentTypeProfiles Int
ctpQuantity = lens _ctpQuantity (\ s a -> s{_ctpQuantity = a})

instance FromXML ContentTypeProfiles where
        parseXML x
          = ContentTypeProfiles' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "ContentTypeProfile"))
                <*> (x .@ "Quantity")

instance Hashable ContentTypeProfiles where

instance NFData ContentTypeProfiles where

instance ToXML ContentTypeProfiles where
        toXML ContentTypeProfiles'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "ContentTypeProfile" <$> _ctpItems),
               "Quantity" @= _ctpQuantity]

-- | A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'cookieNames' smart constructor.
data CookieNames = CookieNames'
  { _cnItems    :: !(Maybe [Text])
  , _cnQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CookieNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnItems' - A complex type that contains one @Name@ element for each cookie that you want CloudFront to forward to the origin for this cache behavior.
--
-- * 'cnQuantity' - The number of different cookies that you want CloudFront to forward to the origin for this cache behavior.
cookieNames
    :: Int -- ^ 'cnQuantity'
    -> CookieNames
cookieNames pQuantity_ =
  CookieNames' {_cnItems = Nothing, _cnQuantity = pQuantity_}


-- | A complex type that contains one @Name@ element for each cookie that you want CloudFront to forward to the origin for this cache behavior.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\ s a -> s{_cnItems = a}) . _Default . _Coerce

-- | The number of different cookies that you want CloudFront to forward to the origin for this cache behavior.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\ s a -> s{_cnQuantity = a})

instance FromXML CookieNames where
        parseXML x
          = CookieNames' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance Hashable CookieNames where

instance NFData CookieNames where

instance ToXML CookieNames where
        toXML CookieNames'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _cnItems),
               "Quantity" @= _cnQuantity]

-- | A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'cookiePreference' smart constructor.
data CookiePreference = CookiePreference'
  { _cpWhitelistedNames :: !(Maybe CookieNames)
  , _cpForward          :: !ItemSelection
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CookiePreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpWhitelistedNames' - Required if you specify @whitelist@ for the value of @Forward:@ . A complex type that specifies how many different cookies you want CloudFront to forward to the origin for this cache behavior and, if you want to forward selected cookies, the names of those cookies. If you specify @all@ or none for the value of @Forward@ , omit @WhitelistedNames@ . If you change the value of @Forward@ from @whitelist@ to all or none and you don't delete the @WhitelistedNames@ element and its child elements, CloudFront deletes them automatically. For the current limit on the number of cookie names that you can whitelist for each cache behavior, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
--
-- * 'cpForward' - Specifies which cookies to forward to the origin for this cache behavior: all, none, or the list of cookies specified in the @WhitelistedNames@ complex type. Amazon S3 doesn't process cookies. When the cache behavior is forwarding requests to an Amazon S3 origin, specify none for the @Forward@ element.
cookiePreference
    :: ItemSelection -- ^ 'cpForward'
    -> CookiePreference
cookiePreference pForward_ =
  CookiePreference' {_cpWhitelistedNames = Nothing, _cpForward = pForward_}


-- | Required if you specify @whitelist@ for the value of @Forward:@ . A complex type that specifies how many different cookies you want CloudFront to forward to the origin for this cache behavior and, if you want to forward selected cookies, the names of those cookies. If you specify @all@ or none for the value of @Forward@ , omit @WhitelistedNames@ . If you change the value of @Forward@ from @whitelist@ to all or none and you don't delete the @WhitelistedNames@ element and its child elements, CloudFront deletes them automatically. For the current limit on the number of cookie names that you can whitelist for each cache behavior, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames = lens _cpWhitelistedNames (\ s a -> s{_cpWhitelistedNames = a})

-- | Specifies which cookies to forward to the origin for this cache behavior: all, none, or the list of cookies specified in the @WhitelistedNames@ complex type. Amazon S3 doesn't process cookies. When the cache behavior is forwarding requests to an Amazon S3 origin, specify none for the @Forward@ element.
cpForward :: Lens' CookiePreference ItemSelection
cpForward = lens _cpForward (\ s a -> s{_cpForward = a})

instance FromXML CookiePreference where
        parseXML x
          = CookiePreference' <$>
              (x .@? "WhitelistedNames") <*> (x .@ "Forward")

instance Hashable CookiePreference where

instance NFData CookiePreference where

instance ToXML CookiePreference where
        toXML CookiePreference'{..}
          = mconcat
              ["WhitelistedNames" @= _cpWhitelistedNames,
               "Forward" @= _cpForward]

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
--
-- For more information about custom error pages, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'customErrorResponse' smart constructor.
data CustomErrorResponse = CustomErrorResponse'
  { _ceResponsePagePath   :: !(Maybe Text)
  , _ceResponseCode       :: !(Maybe Text)
  , _ceErrorCachingMinTTL :: !(Maybe Integer)
  , _ceErrorCode          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomErrorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceResponsePagePath' - The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .      * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages. If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ . If you don't want to specify a value, include an empty element, @<ResponsePagePath>@ , in the XML document. We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
--
-- * 'ceResponseCode' - The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down. If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ . If you don't want to specify a value, include an empty element, @<ResponseCode>@ , in the XML document.
--
-- * 'ceErrorCachingMinTTL' - The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available. If you don't want to specify a value, include an empty element, @<ErrorCachingMinTTL>@ , in the XML document. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'ceErrorCode' - The HTTP status code for which you want to specify a custom error page and/or a caching duration.
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


-- | The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .      * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages. If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ . If you don't want to specify a value, include an empty element, @<ResponsePagePath>@ , in the XML document. We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
ceResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
ceResponsePagePath = lens _ceResponsePagePath (\ s a -> s{_ceResponsePagePath = a})

-- | The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down. If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ . If you don't want to specify a value, include an empty element, @<ResponseCode>@ , in the XML document.
ceResponseCode :: Lens' CustomErrorResponse (Maybe Text)
ceResponseCode = lens _ceResponseCode (\ s a -> s{_ceResponseCode = a})

-- | The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available. If you don't want to specify a value, include an empty element, @<ErrorCachingMinTTL>@ , in the XML document. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
ceErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
ceErrorCachingMinTTL = lens _ceErrorCachingMinTTL (\ s a -> s{_ceErrorCachingMinTTL = a})

-- | The HTTP status code for which you want to specify a custom error page and/or a caching duration.
ceErrorCode :: Lens' CustomErrorResponse Int
ceErrorCode = lens _ceErrorCode (\ s a -> s{_ceErrorCode = a})

instance FromXML CustomErrorResponse where
        parseXML x
          = CustomErrorResponse' <$>
              (x .@? "ResponsePagePath") <*> (x .@? "ResponseCode")
                <*> (x .@? "ErrorCachingMinTTL")
                <*> (x .@ "ErrorCode")

instance Hashable CustomErrorResponse where

instance NFData CustomErrorResponse where

instance ToXML CustomErrorResponse where
        toXML CustomErrorResponse'{..}
          = mconcat
              ["ResponsePagePath" @= _ceResponsePagePath,
               "ResponseCode" @= _ceResponseCode,
               "ErrorCachingMinTTL" @= _ceErrorCachingMinTTL,
               "ErrorCode" @= _ceErrorCode]

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
--
-- For more information about custom error pages, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'customErrorResponses' smart constructor.
data CustomErrorResponses = CustomErrorResponses'
  { _cerItems    :: !(Maybe [CustomErrorResponse])
  , _cerQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomErrorResponses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerItems' - A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- * 'cerQuantity' - The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
customErrorResponses
    :: Int -- ^ 'cerQuantity'
    -> CustomErrorResponses
customErrorResponses pQuantity_ =
  CustomErrorResponses' {_cerItems = Nothing, _cerQuantity = pQuantity_}


-- | A complex type that contains a @CustomErrorResponse@ element for each HTTP status code for which you want to specify a custom error page and/or a caching duration.
cerItems :: Lens' CustomErrorResponses [CustomErrorResponse]
cerItems = lens _cerItems (\ s a -> s{_cerItems = a}) . _Default . _Coerce

-- | The number of HTTP status codes for which you want to specify a custom error page and/or a caching duration. If @Quantity@ is @0@ , you can omit @Items@ .
cerQuantity :: Lens' CustomErrorResponses Int
cerQuantity = lens _cerQuantity (\ s a -> s{_cerQuantity = a})

instance FromXML CustomErrorResponses where
        parseXML x
          = CustomErrorResponses' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CustomErrorResponse"))
                <*> (x .@ "Quantity")

instance Hashable CustomErrorResponses where

instance NFData CustomErrorResponses where

instance ToXML CustomErrorResponses where
        toXML CustomErrorResponses'{..}
          = mconcat
              ["Items" @=
                 toXML
                   (toXMLList "CustomErrorResponse" <$> _cerItems),
               "Quantity" @= _cerQuantity]

-- | A complex type that contains the list of Custom Headers for each origin.
--
--
--
-- /See:/ 'customHeaders' smart constructor.
data CustomHeaders = CustomHeaders'
  { _chItems    :: !(Maybe [OriginCustomHeader])
  , _chQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomHeaders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chItems' - __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
--
-- * 'chQuantity' - The number of custom headers, if any, for this distribution.
customHeaders
    :: Int -- ^ 'chQuantity'
    -> CustomHeaders
customHeaders pQuantity_ =
  CustomHeaders' {_chItems = Nothing, _chQuantity = pQuantity_}


-- | __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
chItems :: Lens' CustomHeaders [OriginCustomHeader]
chItems = lens _chItems (\ s a -> s{_chItems = a}) . _Default . _Coerce

-- | The number of custom headers, if any, for this distribution.
chQuantity :: Lens' CustomHeaders Int
chQuantity = lens _chQuantity (\ s a -> s{_chQuantity = a})

instance FromXML CustomHeaders where
        parseXML x
          = CustomHeaders' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "OriginCustomHeader"))
                <*> (x .@ "Quantity")

instance Hashable CustomHeaders where

instance NFData CustomHeaders where

instance ToXML CustomHeaders where
        toXML CustomHeaders'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "OriginCustomHeader" <$> _chItems),
               "Quantity" @= _chQuantity]

-- | A customer origin.
--
--
--
-- /See:/ 'customOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
  { _cocOriginKeepaliveTimeout :: !(Maybe Int)
  , _cocOriginReadTimeout      :: !(Maybe Int)
  , _cocOriginSSLProtocols     :: !(Maybe OriginSSLProtocols)
  , _cocHTTPPort               :: !Int
  , _cocHTTPSPort              :: !Int
  , _cocOriginProtocolPolicy   :: !OriginProtocolPolicy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomOriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cocOriginKeepaliveTimeout' - You can create a custom keep-alive timeout. All timeout units are in seconds. The default keep-alive timeout is 5 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 1 second; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
--
-- * 'cocOriginReadTimeout' - You can create a custom origin read timeout. All timeout units are in seconds. The default origin read timeout is 30 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 4 seconds; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
--
-- * 'cocOriginSSLProtocols' - The SSL/TLS protocols that you want CloudFront to use when communicating with your origin over HTTPS.
--
-- * 'cocHTTPPort' - The HTTP port the custom origin listens on.
--
-- * 'cocHTTPSPort' - The HTTPS port the custom origin listens on.
--
-- * 'cocOriginProtocolPolicy' - The origin protocol policy to apply to your origin.
customOriginConfig
    :: Int -- ^ 'cocHTTPPort'
    -> Int -- ^ 'cocHTTPSPort'
    -> OriginProtocolPolicy -- ^ 'cocOriginProtocolPolicy'
    -> CustomOriginConfig
customOriginConfig pHTTPPort_ pHTTPSPort_ pOriginProtocolPolicy_ =
  CustomOriginConfig'
    { _cocOriginKeepaliveTimeout = Nothing
    , _cocOriginReadTimeout = Nothing
    , _cocOriginSSLProtocols = Nothing
    , _cocHTTPPort = pHTTPPort_
    , _cocHTTPSPort = pHTTPSPort_
    , _cocOriginProtocolPolicy = pOriginProtocolPolicy_
    }


-- | You can create a custom keep-alive timeout. All timeout units are in seconds. The default keep-alive timeout is 5 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 1 second; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
cocOriginKeepaliveTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginKeepaliveTimeout = lens _cocOriginKeepaliveTimeout (\ s a -> s{_cocOriginKeepaliveTimeout = a})

-- | You can create a custom origin read timeout. All timeout units are in seconds. The default origin read timeout is 30 seconds, but you can configure custom timeout lengths using the CloudFront API. The minimum timeout length is 4 seconds; the maximum is 60 seconds. If you need to increase the maximum time limit, contact the <https://console.aws.amazon.com/support/home#/ AWS Support Center> .
cocOriginReadTimeout :: Lens' CustomOriginConfig (Maybe Int)
cocOriginReadTimeout = lens _cocOriginReadTimeout (\ s a -> s{_cocOriginReadTimeout = a})

-- | The SSL/TLS protocols that you want CloudFront to use when communicating with your origin over HTTPS.
cocOriginSSLProtocols :: Lens' CustomOriginConfig (Maybe OriginSSLProtocols)
cocOriginSSLProtocols = lens _cocOriginSSLProtocols (\ s a -> s{_cocOriginSSLProtocols = a})

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\ s a -> s{_cocHTTPPort = a})

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\ s a -> s{_cocHTTPSPort = a})

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\ s a -> s{_cocOriginProtocolPolicy = a})

instance FromXML CustomOriginConfig where
        parseXML x
          = CustomOriginConfig' <$>
              (x .@? "OriginKeepaliveTimeout") <*>
                (x .@? "OriginReadTimeout")
                <*> (x .@? "OriginSslProtocols")
                <*> (x .@ "HTTPPort")
                <*> (x .@ "HTTPSPort")
                <*> (x .@ "OriginProtocolPolicy")

instance Hashable CustomOriginConfig where

instance NFData CustomOriginConfig where

instance ToXML CustomOriginConfig where
        toXML CustomOriginConfig'{..}
          = mconcat
              ["OriginKeepaliveTimeout" @=
                 _cocOriginKeepaliveTimeout,
               "OriginReadTimeout" @= _cocOriginReadTimeout,
               "OriginSslProtocols" @= _cocOriginSSLProtocols,
               "HTTPPort" @= _cocHTTPPort,
               "HTTPSPort" @= _cocHTTPSPort,
               "OriginProtocolPolicy" @= _cocOriginProtocolPolicy]

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
--
--
-- /See:/ 'defaultCacheBehavior' smart constructor.
data DefaultCacheBehavior = DefaultCacheBehavior'
  { _dcbAllowedMethods             :: !(Maybe AllowedMethods)
  , _dcbLambdaFunctionAssociations :: !(Maybe LambdaFunctionAssociations)
  , _dcbMaxTTL                     :: !(Maybe Integer)
  , _dcbCompress                   :: !(Maybe Bool)
  , _dcbSmoothStreaming            :: !(Maybe Bool)
  , _dcbDefaultTTL                 :: !(Maybe Integer)
  , _dcbFieldLevelEncryptionId     :: !(Maybe Text)
  , _dcbTargetOriginId             :: !Text
  , _dcbForwardedValues            :: !ForwardedValues
  , _dcbTrustedSigners             :: !TrustedSigners
  , _dcbViewerProtocolPolicy       :: !ViewerProtocolPolicy
  , _dcbMinTTL                     :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DefaultCacheBehavior' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbAllowedMethods' - Undocumented member.
--
-- * 'dcbLambdaFunctionAssociations' - A complex type that contains zero or more Lambda function associations for a cache behavior.
--
-- * 'dcbMaxTTL' - Undocumented member.
--
-- * 'dcbCompress' - Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbSmoothStreaming' - Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
--
-- * 'dcbDefaultTTL' - The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbFieldLevelEncryptionId' - Undocumented member.
--
-- * 'dcbTargetOriginId' - The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior.
--
-- * 'dcbForwardedValues' - A complex type that specifies how CloudFront handles query strings and cookies.
--
-- * 'dcbTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- * 'dcbViewerProtocolPolicy' - The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden). For more information about requiring the HTTPS protocol, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcbMinTTL' - The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
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
    , _dcbLambdaFunctionAssociations = Nothing
    , _dcbMaxTTL = Nothing
    , _dcbCompress = Nothing
    , _dcbSmoothStreaming = Nothing
    , _dcbDefaultTTL = Nothing
    , _dcbFieldLevelEncryptionId = Nothing
    , _dcbTargetOriginId = pTargetOriginId_
    , _dcbForwardedValues = pForwardedValues_
    , _dcbTrustedSigners = pTrustedSigners_
    , _dcbViewerProtocolPolicy = pViewerProtocolPolicy_
    , _dcbMinTTL = pMinTTL_
    }


-- | Undocumented member.
dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods = lens _dcbAllowedMethods (\ s a -> s{_dcbAllowedMethods = a})

-- | A complex type that contains zero or more Lambda function associations for a cache behavior.
dcbLambdaFunctionAssociations :: Lens' DefaultCacheBehavior (Maybe LambdaFunctionAssociations)
dcbLambdaFunctionAssociations = lens _dcbLambdaFunctionAssociations (\ s a -> s{_dcbLambdaFunctionAssociations = a})

-- | Undocumented member.
dcbMaxTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbMaxTTL = lens _dcbMaxTTL (\ s a -> s{_dcbMaxTTL = a})

-- | Whether you want CloudFront to automatically compress certain files for this cache behavior. If so, specify @true@ ; if not, specify @false@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/ServingCompressedFiles.html Serving Compressed Files> in the /Amazon CloudFront Developer Guide/ .
dcbCompress :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbCompress = lens _dcbCompress (\ s a -> s{_dcbCompress = a})

-- | Indicates whether you want to distribute media files in the Microsoft Smooth Streaming format using the origin that is associated with this cache behavior. If so, specify @true@ ; if not, specify @false@ . If you specify @true@ for @SmoothStreaming@ , you can still distribute other content using this cache behavior if the content matches the value of @PathPattern@ .
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming = lens _dcbSmoothStreaming (\ s a -> s{_dcbSmoothStreaming = a})

-- | The default amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. The value that you specify applies only when your origin does not add HTTP headers such as @Cache-Control max-age@ , @Cache-Control s-maxage@ , and @Expires@ to objects. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon CloudFront Developer Guide/ .
dcbDefaultTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbDefaultTTL = lens _dcbDefaultTTL (\ s a -> s{_dcbDefaultTTL = a})

-- | Undocumented member.
dcbFieldLevelEncryptionId :: Lens' DefaultCacheBehavior (Maybe Text)
dcbFieldLevelEncryptionId = lens _dcbFieldLevelEncryptionId (\ s a -> s{_dcbFieldLevelEncryptionId = a})

-- | The value of @ID@ for the origin that you want CloudFront to route requests to when a request matches the path pattern either for a cache behavior or for the default cache behavior.
dcbTargetOriginId :: Lens' DefaultCacheBehavior Text
dcbTargetOriginId = lens _dcbTargetOriginId (\ s a -> s{_dcbTargetOriginId = a})

-- | A complex type that specifies how CloudFront handles query strings and cookies.
dcbForwardedValues :: Lens' DefaultCacheBehavior ForwardedValues
dcbForwardedValues = lens _dcbForwardedValues (\ s a -> s{_dcbForwardedValues = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon Amazon CloudFront Developer Guide/ . If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior TrustedSigners
dcbTrustedSigners = lens _dcbTrustedSigners (\ s a -> s{_dcbTrustedSigners = a})

-- | The protocol that viewers can use to access the files in the origin specified by @TargetOriginId@ when a request matches the path pattern in @PathPattern@ . You can specify the following options:     * @allow-all@ : Viewers can use HTTP or HTTPS.     * @redirect-to-https@ : If a viewer submits an HTTP request, CloudFront returns an HTTP status code of 301 (Moved Permanently) to the viewer along with the HTTPS URL. The viewer then resubmits the request using the new URL.     * @https-only@ : If a viewer sends an HTTP request, CloudFront returns an HTTP status code of 403 (Forbidden). For more information about requiring the HTTPS protocol, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html Using an HTTPS Connection to Access Your Objects> in the /Amazon CloudFront Developer Guide/ .
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior ViewerProtocolPolicy
dcbViewerProtocolPolicy = lens _dcbViewerProtocolPolicy (\ s a -> s{_dcbViewerProtocolPolicy = a})

-- | The minimum amount of time that you want objects to stay in CloudFront caches before CloudFront forwards another request to your origin to determine whether the object has been updated. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Specifying How Long Objects and Errors Stay in a CloudFront Edge Cache (Expiration)> in the /Amazon Amazon CloudFront Developer Guide/ . You must specify @0@ for @MinTTL@ if you configure CloudFront to forward all headers to your origin (under @Headers@ , if you specify @1@ for @Quantity@ and @*@ for @Name@ ).
dcbMinTTL :: Lens' DefaultCacheBehavior Integer
dcbMinTTL = lens _dcbMinTTL (\ s a -> s{_dcbMinTTL = a})

instance FromXML DefaultCacheBehavior where
        parseXML x
          = DefaultCacheBehavior' <$>
              (x .@? "AllowedMethods") <*>
                (x .@? "LambdaFunctionAssociations")
                <*> (x .@? "MaxTTL")
                <*> (x .@? "Compress")
                <*> (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@? "FieldLevelEncryptionId")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance Hashable DefaultCacheBehavior where

instance NFData DefaultCacheBehavior where

instance ToXML DefaultCacheBehavior where
        toXML DefaultCacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _dcbAllowedMethods,
               "LambdaFunctionAssociations" @=
                 _dcbLambdaFunctionAssociations,
               "MaxTTL" @= _dcbMaxTTL, "Compress" @= _dcbCompress,
               "SmoothStreaming" @= _dcbSmoothStreaming,
               "DefaultTTL" @= _dcbDefaultTTL,
               "FieldLevelEncryptionId" @=
                 _dcbFieldLevelEncryptionId,
               "TargetOriginId" @= _dcbTargetOriginId,
               "ForwardedValues" @= _dcbForwardedValues,
               "TrustedSigners" @= _dcbTrustedSigners,
               "ViewerProtocolPolicy" @= _dcbViewerProtocolPolicy,
               "MinTTL" @= _dcbMinTTL]

-- | The distribution's information.
--
--
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'
  { _dId                            :: !Text
  , _dARN                           :: !Text
  , _dStatus                        :: !Text
  , _dLastModifiedTime              :: !ISO8601
  , _dInProgressInvalidationBatches :: !Int
  , _dDomainName                    :: !Text
  , _dActiveTrustedSigners          :: !ActiveTrustedSigners
  , _dDistributionConfig            :: !DistributionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dId' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- * 'dARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'dStatus' - This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
--
-- * 'dLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'dInProgressInvalidationBatches' - The number of invalidation batches currently in progress.
--
-- * 'dDomainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'dActiveTrustedSigners' - CloudFront automatically adds this element to the response only if you've set up the distribution to serve private content with signed URLs. The element lists the key pair IDs that CloudFront is aware of for each trusted signer. The @Signer@ child element lists the AWS account number of the trusted signer (or an empty @Self@ element if the signer is you). The @Signer@ element also includes the IDs of any active key pairs associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create working signed URLs.
--
-- * 'dDistributionConfig' - The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
distribution
    :: Text -- ^ 'dId'
    -> Text -- ^ 'dARN'
    -> Text -- ^ 'dStatus'
    -> UTCTime -- ^ 'dLastModifiedTime'
    -> Int -- ^ 'dInProgressInvalidationBatches'
    -> Text -- ^ 'dDomainName'
    -> ActiveTrustedSigners -- ^ 'dActiveTrustedSigners'
    -> DistributionConfig -- ^ 'dDistributionConfig'
    -> Distribution
distribution pId_ pARN_ pStatus_ pLastModifiedTime_ pInProgressInvalidationBatches_ pDomainName_ pActiveTrustedSigners_ pDistributionConfig_ =
  Distribution'
    { _dId = pId_
    , _dARN = pARN_
    , _dStatus = pStatus_
    , _dLastModifiedTime = _Time # pLastModifiedTime_
    , _dInProgressInvalidationBatches = pInProgressInvalidationBatches_
    , _dDomainName = pDomainName_
    , _dActiveTrustedSigners = pActiveTrustedSigners_
    , _dDistributionConfig = pDistributionConfig_
    }


-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
dId :: Lens' Distribution Text
dId = lens _dId (\ s a -> s{_dId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
dARN :: Lens' Distribution Text
dARN = lens _dARN (\ s a -> s{_dARN = a})

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations.
dStatus :: Lens' Distribution Text
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The date and time the distribution was last modified.
dLastModifiedTime :: Lens' Distribution UTCTime
dLastModifiedTime = lens _dLastModifiedTime (\ s a -> s{_dLastModifiedTime = a}) . _Time

-- | The number of invalidation batches currently in progress.
dInProgressInvalidationBatches :: Lens' Distribution Int
dInProgressInvalidationBatches = lens _dInProgressInvalidationBatches (\ s a -> s{_dInProgressInvalidationBatches = a})

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
dDomainName :: Lens' Distribution Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a})

-- | CloudFront automatically adds this element to the response only if you've set up the distribution to serve private content with signed URLs. The element lists the key pair IDs that CloudFront is aware of for each trusted signer. The @Signer@ child element lists the AWS account number of the trusted signer (or an empty @Self@ element if the signer is you). The @Signer@ element also includes the IDs of any active key pairs associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create working signed URLs.
dActiveTrustedSigners :: Lens' Distribution ActiveTrustedSigners
dActiveTrustedSigners = lens _dActiveTrustedSigners (\ s a -> s{_dActiveTrustedSigners = a})

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
dDistributionConfig :: Lens' Distribution DistributionConfig
dDistributionConfig = lens _dDistributionConfig (\ s a -> s{_dDistributionConfig = a})

instance FromXML Distribution where
        parseXML x
          = Distribution' <$>
              (x .@ "Id") <*> (x .@ "ARN") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "InProgressInvalidationBatches")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "DistributionConfig")

instance Hashable Distribution where

instance NFData Distribution where

-- | A distribution configuration.
--
--
--
-- /See:/ 'distributionConfig' smart constructor.
data DistributionConfig = DistributionConfig'
  { _dcHTTPVersion          :: !(Maybe HTTPVersion)
  , _dcAliases              :: !(Maybe Aliases)
  , _dcDefaultRootObject    :: !(Maybe Text)
  , _dcPriceClass           :: !(Maybe PriceClass)
  , _dcCustomErrorResponses :: !(Maybe CustomErrorResponses)
  , _dcWebACLId             :: !(Maybe Text)
  , _dcViewerCertificate    :: !(Maybe ViewerCertificate)
  , _dcRestrictions         :: !(Maybe Restrictions)
  , _dcLogging              :: !(Maybe LoggingConfig)
  , _dcCacheBehaviors       :: !(Maybe CacheBehaviors)
  , _dcIsIPV6Enabled        :: !(Maybe Bool)
  , _dcCallerReference      :: !Text
  , _dcOrigins              :: !Origins
  , _dcDefaultCacheBehavior :: !DefaultCacheBehavior
  , _dcComment              :: !Text
  , _dcEnabled              :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcHTTPVersion' - (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version. For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI). In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
--
-- * 'dcAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- * 'dcDefaultRootObject' - The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution. Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name. If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element. To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element. To replace the default root object, update the distribution configuration and specify the new object. For more information about the default root object, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcPriceClass' - The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations. If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance. For more information about price classes, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes map to CloudFront regions, see <https://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
--
-- * 'dcCustomErrorResponses' - A complex type that controls the following:     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range. For more information about custom error pages, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcWebACLId' - A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <http://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
--
-- * 'dcViewerCertificate' - Undocumented member.
--
-- * 'dcRestrictions' - Undocumented member.
--
-- * 'dcLogging' - A complex type that controls whether access logs are written for the distribution. For more information about logging, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'dcCacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- * 'dcIsIPV6Enabled' - If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.  In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ . If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:     * You enable IPv6 for the distribution     * You're using alternate domain names in the URLs for your objects For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ . If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
--
-- * 'dcCallerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value you already sent in a previous request to create a distribution, and if the content of the @DistributionConfig@ is identical to the original request (ignoring white space), CloudFront returns the same the response that it returned to the original request. If @CallerReference@ is a value you already sent in a previous request to create a distribution but the content of the @DistributionConfig@ is different from the original request, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- * 'dcOrigins' - A complex type that contains information about origins for this distribution.
--
-- * 'dcDefaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- * 'dcComment' - Any comments you want to include about the distribution. If you don't want to specify a comment, include an empty @Comment@ element. To delete an existing comment, update the distribution configuration and include an empty @Comment@ element. To add or change a comment, update the distribution configuration and specify the new comment.
--
-- * 'dcEnabled' - From this field, you can enable or disable the selected distribution. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
distributionConfig
    :: Text -- ^ 'dcCallerReference'
    -> Origins -- ^ 'dcOrigins'
    -> DefaultCacheBehavior -- ^ 'dcDefaultCacheBehavior'
    -> Text -- ^ 'dcComment'
    -> Bool -- ^ 'dcEnabled'
    -> DistributionConfig
distributionConfig pCallerReference_ pOrigins_ pDefaultCacheBehavior_ pComment_ pEnabled_ =
  DistributionConfig'
    { _dcHTTPVersion = Nothing
    , _dcAliases = Nothing
    , _dcDefaultRootObject = Nothing
    , _dcPriceClass = Nothing
    , _dcCustomErrorResponses = Nothing
    , _dcWebACLId = Nothing
    , _dcViewerCertificate = Nothing
    , _dcRestrictions = Nothing
    , _dcLogging = Nothing
    , _dcCacheBehaviors = Nothing
    , _dcIsIPV6Enabled = Nothing
    , _dcCallerReference = pCallerReference_
    , _dcOrigins = pOrigins_
    , _dcDefaultCacheBehavior = pDefaultCacheBehavior_
    , _dcComment = pComment_
    , _dcEnabled = pEnabled_
    }


-- | (Optional) Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is http2. Viewers that don't support HTTP/2 automatically use an earlier HTTP version. For viewers and CloudFront to use HTTP/2, viewers must support TLS 1.2 or later, and must support Server Name Identification (SNI). In general, configuring CloudFront to communicate with viewers using HTTP/2 reduces latency. You can improve performance by optimizing for HTTP/2. For more information, do an Internet search for "http/2 optimization."
dcHTTPVersion :: Lens' DistributionConfig (Maybe HTTPVersion)
dcHTTPVersion = lens _dcHTTPVersion (\ s a -> s{_dcHTTPVersion = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Maybe Aliases)
dcAliases = lens _dcAliases (\ s a -> s{_dcAliases = a})

-- | The object that you want CloudFront to request from your origin (for example, @index.html@ ) when a viewer requests the root URL for your distribution (@http://www.example.com@ ) instead of an object in your distribution (@http://www.example.com/product-description.html@ ). Specifying a default root object avoids exposing the contents of your distribution. Specify only the object name, for example, @index.html@ . Don't add a @/@ before the object name. If you don't want to specify a default root object when you create a distribution, include an empty @DefaultRootObject@ element. To delete the default root object from an existing distribution, update the distribution configuration and include an empty @DefaultRootObject@ element. To replace the default root object, update the distribution configuration and specify the new object. For more information about the default root object, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/DefaultRootObject.html Creating a Default Root Object> in the /Amazon CloudFront Developer Guide/ .
dcDefaultRootObject :: Lens' DistributionConfig (Maybe Text)
dcDefaultRootObject = lens _dcDefaultRootObject (\ s a -> s{_dcDefaultRootObject = a})

-- | The price class that corresponds with the maximum price that you want to pay for CloudFront service. If you specify @PriceClass_All@ , CloudFront responds to requests for your objects from all CloudFront edge locations. If you specify a price class other than @PriceClass_All@ , CloudFront serves your objects from the CloudFront edge location that has the lowest latency among the edge locations in your price class. Viewers who are in or near regions that are excluded from your specified price class may encounter slower performance. For more information about price classes, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PriceClass.html Choosing the Price Class for a CloudFront Distribution> in the /Amazon CloudFront Developer Guide/ . For information about CloudFront pricing, including how price classes map to CloudFront regions, see <https://aws.amazon.com/cloudfront/pricing/ Amazon CloudFront Pricing> .
dcPriceClass :: Lens' DistributionConfig (Maybe PriceClass)
dcPriceClass = lens _dcPriceClass (\ s a -> s{_dcPriceClass = a})

-- | A complex type that controls the following:     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range. For more information about custom error pages, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses = lens _dcCustomErrorResponses (\ s a -> s{_dcCustomErrorResponses = a})

-- | A unique identifier that specifies the AWS WAF web ACL, if any, to associate with this distribution. AWS WAF is a web application firewall that lets you monitor the HTTP and HTTPS requests that are forwarded to CloudFront, and lets you control access to your content. Based on conditions that you specify, such as the IP addresses that requests originate from or the values of query strings, CloudFront responds to requests either with the requested content or with an HTTP 403 status code (Forbidden). You can also configure CloudFront to return a custom error page when a request is blocked. For more information about AWS WAF, see the <http://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html AWS WAF Developer Guide> .
dcWebACLId :: Lens' DistributionConfig (Maybe Text)
dcWebACLId = lens _dcWebACLId (\ s a -> s{_dcWebACLId = a})

-- | Undocumented member.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate = lens _dcViewerCertificate (\ s a -> s{_dcViewerCertificate = a})

-- | Undocumented member.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\ s a -> s{_dcRestrictions = a})

-- | A complex type that controls whether access logs are written for the distribution. For more information about logging, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/AccessLogs.html Access Logs> in the /Amazon CloudFront Developer Guide/ .
dcLogging :: Lens' DistributionConfig (Maybe LoggingConfig)
dcLogging = lens _dcLogging (\ s a -> s{_dcLogging = a})

-- | A complex type that contains zero or more @CacheBehavior@ elements.
dcCacheBehaviors :: Lens' DistributionConfig (Maybe CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\ s a -> s{_dcCacheBehaviors = a})

-- | If you want CloudFront to respond to IPv6 DNS requests with an IPv6 address for your distribution, specify @true@ . If you specify @false@ , CloudFront responds to IPv6 DNS requests with the DNS response code @NOERROR@ and with no IP addresses. This allows viewers to submit a second request, for an IPv4 address for your distribution.  In general, you should enable IPv6 if you have users on IPv6 networks who want to access your content. However, if you're using signed URLs or signed cookies to restrict access to your content, and if you're using a custom policy that includes the @IpAddress@ parameter to restrict the IP addresses that can access your content, don't enable IPv6. If you want to restrict access to some content by IP address and not restrict access to other content (or restrict access but not by IP address), you can create two distributions. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-creating-signed-url-custom-policy.html Creating a Signed URL Using a Custom Policy> in the /Amazon CloudFront Developer Guide/ . If you're using an Amazon Route 53 alias resource record set to route traffic to your CloudFront distribution, you need to create a second alias resource record set when both of the following are true:     * You enable IPv6 for the distribution     * You're using alternate domain names in the URLs for your objects For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-to-cloudfront-distribution.html Routing Traffic to an Amazon CloudFront Web Distribution by Using Your Domain Name> in the /Amazon Route 53 Developer Guide/ . If you created a CNAME resource record set, either with Amazon Route 53 or with another DNS service, you don't need to make any changes. A CNAME record will route traffic to your distribution regardless of the IP address format of the viewer request.
dcIsIPV6Enabled :: Lens' DistributionConfig (Maybe Bool)
dcIsIPV6Enabled = lens _dcIsIPV6Enabled (\ s a -> s{_dcIsIPV6Enabled = a})

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed. If the value of @CallerReference@ is new (regardless of the content of the @DistributionConfig@ object), CloudFront creates a new distribution. If @CallerReference@ is a value you already sent in a previous request to create a distribution, and if the content of the @DistributionConfig@ is identical to the original request (ignoring white space), CloudFront returns the same the response that it returned to the original request. If @CallerReference@ is a value you already sent in a previous request to create a distribution but the content of the @DistributionConfig@ is different from the original request, CloudFront returns a @DistributionAlreadyExists@ error.
dcCallerReference :: Lens' DistributionConfig Text
dcCallerReference = lens _dcCallerReference (\ s a -> s{_dcCallerReference = a})

-- | A complex type that contains information about origins for this distribution.
dcOrigins :: Lens' DistributionConfig Origins
dcOrigins = lens _dcOrigins (\ s a -> s{_dcOrigins = a})

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior = lens _dcDefaultCacheBehavior (\ s a -> s{_dcDefaultCacheBehavior = a})

-- | Any comments you want to include about the distribution. If you don't want to specify a comment, include an empty @Comment@ element. To delete an existing comment, update the distribution configuration and include an empty @Comment@ element. To add or change a comment, update the distribution configuration and specify the new comment.
dcComment :: Lens' DistributionConfig Text
dcComment = lens _dcComment (\ s a -> s{_dcComment = a})

-- | From this field, you can enable or disable the selected distribution. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
dcEnabled :: Lens' DistributionConfig Bool
dcEnabled = lens _dcEnabled (\ s a -> s{_dcEnabled = a})

instance FromXML DistributionConfig where
        parseXML x
          = DistributionConfig' <$>
              (x .@? "HttpVersion") <*> (x .@? "Aliases") <*>
                (x .@? "DefaultRootObject")
                <*> (x .@? "PriceClass")
                <*> (x .@? "CustomErrorResponses")
                <*> (x .@? "WebACLId")
                <*> (x .@? "ViewerCertificate")
                <*> (x .@? "Restrictions")
                <*> (x .@? "Logging")
                <*> (x .@? "CacheBehaviors")
                <*> (x .@? "IsIPV6Enabled")
                <*> (x .@ "CallerReference")
                <*> (x .@ "Origins")
                <*> (x .@ "DefaultCacheBehavior")
                <*> (x .@ "Comment")
                <*> (x .@ "Enabled")

instance Hashable DistributionConfig where

instance NFData DistributionConfig where

instance ToXML DistributionConfig where
        toXML DistributionConfig'{..}
          = mconcat
              ["HttpVersion" @= _dcHTTPVersion,
               "Aliases" @= _dcAliases,
               "DefaultRootObject" @= _dcDefaultRootObject,
               "PriceClass" @= _dcPriceClass,
               "CustomErrorResponses" @= _dcCustomErrorResponses,
               "WebACLId" @= _dcWebACLId,
               "ViewerCertificate" @= _dcViewerCertificate,
               "Restrictions" @= _dcRestrictions,
               "Logging" @= _dcLogging,
               "CacheBehaviors" @= _dcCacheBehaviors,
               "IsIPV6Enabled" @= _dcIsIPV6Enabled,
               "CallerReference" @= _dcCallerReference,
               "Origins" @= _dcOrigins,
               "DefaultCacheBehavior" @= _dcDefaultCacheBehavior,
               "Comment" @= _dcComment, "Enabled" @= _dcEnabled]

-- | A distribution Configuration and a list of tags to be associated with the distribution.
--
--
--
-- /See:/ 'distributionConfigWithTags' smart constructor.
data DistributionConfigWithTags = DistributionConfigWithTags'
  { _dcwtDistributionConfig :: !DistributionConfig
  , _dcwtTags               :: !Tags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DistributionConfigWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcwtDistributionConfig' - A distribution configuration.
--
-- * 'dcwtTags' - A complex type that contains zero or more @Tag@ elements.
distributionConfigWithTags
    :: DistributionConfig -- ^ 'dcwtDistributionConfig'
    -> Tags -- ^ 'dcwtTags'
    -> DistributionConfigWithTags
distributionConfigWithTags pDistributionConfig_ pTags_ =
  DistributionConfigWithTags'
    {_dcwtDistributionConfig = pDistributionConfig_, _dcwtTags = pTags_}


-- | A distribution configuration.
dcwtDistributionConfig :: Lens' DistributionConfigWithTags DistributionConfig
dcwtDistributionConfig = lens _dcwtDistributionConfig (\ s a -> s{_dcwtDistributionConfig = a})

-- | A complex type that contains zero or more @Tag@ elements.
dcwtTags :: Lens' DistributionConfigWithTags Tags
dcwtTags = lens _dcwtTags (\ s a -> s{_dcwtTags = a})

instance Hashable DistributionConfigWithTags where

instance NFData DistributionConfigWithTags where

instance ToXML DistributionConfigWithTags where
        toXML DistributionConfigWithTags'{..}
          = mconcat
              ["DistributionConfig" @= _dcwtDistributionConfig,
               "Tags" @= _dcwtTags]

-- | A distribution list.
--
--
--
-- /See:/ 'distributionList' smart constructor.
data DistributionList = DistributionList'
  { _dlItems       :: !(Maybe [DistributionSummary])
  , _dlNextMarker  :: !(Maybe Text)
  , _dlMarker      :: !Text
  , _dlMaxItems    :: !Int
  , _dlIsTruncated :: !Bool
  , _dlQuantity    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlItems' - A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- * 'dlNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
--
-- * 'dlMarker' - The value you provided for the @Marker@ request parameter.
--
-- * 'dlMaxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- * 'dlIsTruncated' - A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- * 'dlQuantity' - The number of distributions that were created by the current AWS account.
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


-- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList [DistributionSummary]
dlItems = lens _dlItems (\ s a -> s{_dlItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\ s a -> s{_dlNextMarker = a})

-- | The value you provided for the @Marker@ request parameter.
dlMarker :: Lens' DistributionList Text
dlMarker = lens _dlMarker (\ s a -> s{_dlMarker = a})

-- | The value you provided for the @MaxItems@ request parameter.
dlMaxItems :: Lens' DistributionList Int
dlMaxItems = lens _dlMaxItems (\ s a -> s{_dlMaxItems = a})

-- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
dlIsTruncated :: Lens' DistributionList Bool
dlIsTruncated = lens _dlIsTruncated (\ s a -> s{_dlIsTruncated = a})

-- | The number of distributions that were created by the current AWS account.
dlQuantity :: Lens' DistributionList Int
dlQuantity = lens _dlQuantity (\ s a -> s{_dlQuantity = a})

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

instance Hashable DistributionList where

instance NFData DistributionList where

-- | A summary of the information about a CloudFront distribution.
--
--
--
-- /See:/ 'distributionSummary' smart constructor.
data DistributionSummary = DistributionSummary'
  { _dsId                   :: !Text
  , _dsARN                  :: !Text
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
  , _dsHTTPVersion          :: !HTTPVersion
  , _dsIsIPV6Enabled        :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
--
-- * 'dsARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'dsStatus' - The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- * 'dsLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'dsDomainName' - The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'dsAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
-- * 'dsOrigins' - A complex type that contains information about origins for this distribution.
--
-- * 'dsDefaultCacheBehavior' - A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
--
-- * 'dsCacheBehaviors' - A complex type that contains zero or more @CacheBehavior@ elements.
--
-- * 'dsCustomErrorResponses' - A complex type that contains zero or more @CustomErrorResponses@ elements.
--
-- * 'dsComment' - The comment originally specified when this distribution was created.
--
-- * 'dsPriceClass' - Undocumented member.
--
-- * 'dsEnabled' - Whether the distribution is enabled to accept user requests for content.
--
-- * 'dsViewerCertificate' - Undocumented member.
--
-- * 'dsRestrictions' - Undocumented member.
--
-- * 'dsWebACLId' - The Web ACL Id (if any) associated with the distribution.
--
-- * 'dsHTTPVersion' - Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
--
-- * 'dsIsIPV6Enabled' - Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
distributionSummary
    :: Text -- ^ 'dsId'
    -> Text -- ^ 'dsARN'
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
    -> HTTPVersion -- ^ 'dsHTTPVersion'
    -> Bool -- ^ 'dsIsIPV6Enabled'
    -> DistributionSummary
distributionSummary pId_ pARN_ pStatus_ pLastModifiedTime_ pDomainName_ pAliases_ pOrigins_ pDefaultCacheBehavior_ pCacheBehaviors_ pCustomErrorResponses_ pComment_ pPriceClass_ pEnabled_ pViewerCertificate_ pRestrictions_ pWebACLId_ pHTTPVersion_ pIsIPV6Enabled_ =
  DistributionSummary'
    { _dsId = pId_
    , _dsARN = pARN_
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
    , _dsHTTPVersion = pHTTPVersion_
    , _dsIsIPV6Enabled = pIsIPV6Enabled_
    }


-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ .
dsId :: Lens' DistributionSummary Text
dsId = lens _dsId (\ s a -> s{_dsId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
dsARN :: Lens' DistributionSummary Text
dsARN = lens _dsARN (\ s a -> s{_dsARN = a})

-- | The current status of the distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
dsStatus :: Lens' DistributionSummary Text
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a})

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary UTCTime
dsLastModifiedTime = lens _dsLastModifiedTime (\ s a -> s{_dsLastModifiedTime = a}) . _Time

-- | The domain name that corresponds to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
dsDomainName :: Lens' DistributionSummary Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary Aliases
dsAliases = lens _dsAliases (\ s a -> s{_dsAliases = a})

-- | A complex type that contains information about origins for this distribution.
dsOrigins :: Lens' DistributionSummary Origins
dsOrigins = lens _dsOrigins (\ s a -> s{_dsOrigins = a})

-- | A complex type that describes the default cache behavior if you don't specify a @CacheBehavior@ element or if files don't match any of the values of @PathPattern@ in @CacheBehavior@ elements. You must create exactly one default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior = lens _dsDefaultCacheBehavior (\ s a -> s{_dsDefaultCacheBehavior = a})

-- | A complex type that contains zero or more @CacheBehavior@ elements.
dsCacheBehaviors :: Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = lens _dsCacheBehaviors (\ s a -> s{_dsCacheBehaviors = a})

-- | A complex type that contains zero or more @CustomErrorResponses@ elements.
dsCustomErrorResponses :: Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses = lens _dsCustomErrorResponses (\ s a -> s{_dsCustomErrorResponses = a})

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary Text
dsComment = lens _dsComment (\ s a -> s{_dsComment = a})

-- | Undocumented member.
dsPriceClass :: Lens' DistributionSummary PriceClass
dsPriceClass = lens _dsPriceClass (\ s a -> s{_dsPriceClass = a})

-- | Whether the distribution is enabled to accept user requests for content.
dsEnabled :: Lens' DistributionSummary Bool
dsEnabled = lens _dsEnabled (\ s a -> s{_dsEnabled = a})

-- | Undocumented member.
dsViewerCertificate :: Lens' DistributionSummary ViewerCertificate
dsViewerCertificate = lens _dsViewerCertificate (\ s a -> s{_dsViewerCertificate = a})

-- | Undocumented member.
dsRestrictions :: Lens' DistributionSummary Restrictions
dsRestrictions = lens _dsRestrictions (\ s a -> s{_dsRestrictions = a})

-- | The Web ACL Id (if any) associated with the distribution.
dsWebACLId :: Lens' DistributionSummary Text
dsWebACLId = lens _dsWebACLId (\ s a -> s{_dsWebACLId = a})

-- | Specify the maximum HTTP version that you want viewers to use to communicate with CloudFront. The default value for new web distributions is @http2@ . Viewers that don't support @HTTP/2@ will automatically use an earlier version.
dsHTTPVersion :: Lens' DistributionSummary HTTPVersion
dsHTTPVersion = lens _dsHTTPVersion (\ s a -> s{_dsHTTPVersion = a})

-- | Whether CloudFront responds to IPv6 DNS requests with an IPv6 address for your distribution.
dsIsIPV6Enabled :: Lens' DistributionSummary Bool
dsIsIPV6Enabled = lens _dsIsIPV6Enabled (\ s a -> s{_dsIsIPV6Enabled = a})

instance FromXML DistributionSummary where
        parseXML x
          = DistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "ARN") <*> (x .@ "Status") <*>
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
                <*> (x .@ "HttpVersion")
                <*> (x .@ "IsIPV6Enabled")

instance Hashable DistributionSummary where

instance NFData DistributionSummary where

-- | Complex data type for field-level encryption profiles that includes all of the encryption entities.
--
--
--
-- /See:/ 'encryptionEntities' smart constructor.
data EncryptionEntities = EncryptionEntities'
  { _eeItems    :: !(Maybe [EncryptionEntity])
  , _eeQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeItems' - An array of field patterns in a field-level encryption content type-profile mapping.
--
-- * 'eeQuantity' - Number of field pattern items in a field-level encryption content type-profile mapping.
encryptionEntities
    :: Int -- ^ 'eeQuantity'
    -> EncryptionEntities
encryptionEntities pQuantity_ =
  EncryptionEntities' {_eeItems = Nothing, _eeQuantity = pQuantity_}


-- | An array of field patterns in a field-level encryption content type-profile mapping.
eeItems :: Lens' EncryptionEntities [EncryptionEntity]
eeItems = lens _eeItems (\ s a -> s{_eeItems = a}) . _Default . _Coerce

-- | Number of field pattern items in a field-level encryption content type-profile mapping.
eeQuantity :: Lens' EncryptionEntities Int
eeQuantity = lens _eeQuantity (\ s a -> s{_eeQuantity = a})

instance FromXML EncryptionEntities where
        parseXML x
          = EncryptionEntities' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "EncryptionEntity"))
                <*> (x .@ "Quantity")

instance Hashable EncryptionEntities where

instance NFData EncryptionEntities where

instance ToXML EncryptionEntities where
        toXML EncryptionEntities'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "EncryptionEntity" <$> _eeItems),
               "Quantity" @= _eeQuantity]

-- | Complex data type for field-level encryption profiles that includes the encryption key and field pattern specifications.
--
--
--
-- /See:/ 'encryptionEntity' smart constructor.
data EncryptionEntity = EncryptionEntity'
  { _eePublicKeyId   :: !Text
  , _eeProviderId    :: !Text
  , _eeFieldPatterns :: !FieldPatterns
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eePublicKeyId' - The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
--
-- * 'eeProviderId' - The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
--
-- * 'eeFieldPatterns' - Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
encryptionEntity
    :: Text -- ^ 'eePublicKeyId'
    -> Text -- ^ 'eeProviderId'
    -> FieldPatterns -- ^ 'eeFieldPatterns'
    -> EncryptionEntity
encryptionEntity pPublicKeyId_ pProviderId_ pFieldPatterns_ =
  EncryptionEntity'
    { _eePublicKeyId = pPublicKeyId_
    , _eeProviderId = pProviderId_
    , _eeFieldPatterns = pFieldPatterns_
    }


-- | The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
eePublicKeyId :: Lens' EncryptionEntity Text
eePublicKeyId = lens _eePublicKeyId (\ s a -> s{_eePublicKeyId = a})

-- | The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
eeProviderId :: Lens' EncryptionEntity Text
eeProviderId = lens _eeProviderId (\ s a -> s{_eeProviderId = a})

-- | Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
eeFieldPatterns :: Lens' EncryptionEntity FieldPatterns
eeFieldPatterns = lens _eeFieldPatterns (\ s a -> s{_eeFieldPatterns = a})

instance FromXML EncryptionEntity where
        parseXML x
          = EncryptionEntity' <$>
              (x .@ "PublicKeyId") <*> (x .@ "ProviderId") <*>
                (x .@ "FieldPatterns")

instance Hashable EncryptionEntity where

instance NFData EncryptionEntity where

instance ToXML EncryptionEntity where
        toXML EncryptionEntity'{..}
          = mconcat
              ["PublicKeyId" @= _eePublicKeyId,
               "ProviderId" @= _eeProviderId,
               "FieldPatterns" @= _eeFieldPatterns]

-- | A complex data type that includes the profile configurations and other options specified for field-level encryption.
--
--
--
-- /See:/ 'fieldLevelEncryption' smart constructor.
data FieldLevelEncryption = FieldLevelEncryption'
  { _fleId                         :: !Text
  , _fleLastModifiedTime           :: !ISO8601
  , _fleFieldLevelEncryptionConfig :: !FieldLevelEncryptionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleId' - The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- * 'fleLastModifiedTime' - The last time the field-level encryption configuration was changed.
--
-- * 'fleFieldLevelEncryptionConfig' - A complex data type that includes the profile configurations specified for field-level encryption.
fieldLevelEncryption
    :: Text -- ^ 'fleId'
    -> UTCTime -- ^ 'fleLastModifiedTime'
    -> FieldLevelEncryptionConfig -- ^ 'fleFieldLevelEncryptionConfig'
    -> FieldLevelEncryption
fieldLevelEncryption pId_ pLastModifiedTime_ pFieldLevelEncryptionConfig_ =
  FieldLevelEncryption'
    { _fleId = pId_
    , _fleLastModifiedTime = _Time # pLastModifiedTime_
    , _fleFieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_
    }


-- | The configuration ID for a field-level encryption configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
fleId :: Lens' FieldLevelEncryption Text
fleId = lens _fleId (\ s a -> s{_fleId = a})

-- | The last time the field-level encryption configuration was changed.
fleLastModifiedTime :: Lens' FieldLevelEncryption UTCTime
fleLastModifiedTime = lens _fleLastModifiedTime (\ s a -> s{_fleLastModifiedTime = a}) . _Time

-- | A complex data type that includes the profile configurations specified for field-level encryption.
fleFieldLevelEncryptionConfig :: Lens' FieldLevelEncryption FieldLevelEncryptionConfig
fleFieldLevelEncryptionConfig = lens _fleFieldLevelEncryptionConfig (\ s a -> s{_fleFieldLevelEncryptionConfig = a})

instance FromXML FieldLevelEncryption where
        parseXML x
          = FieldLevelEncryption' <$>
              (x .@ "Id") <*> (x .@ "LastModifiedTime") <*>
                (x .@ "FieldLevelEncryptionConfig")

instance Hashable FieldLevelEncryption where

instance NFData FieldLevelEncryption where

-- | A complex data type that includes the profile configurations specified for field-level encryption.
--
--
--
-- /See:/ 'fieldLevelEncryptionConfig' smart constructor.
data FieldLevelEncryptionConfig = FieldLevelEncryptionConfig'
  { _flecQueryArgProfileConfig    :: !(Maybe QueryArgProfileConfig)
  , _flecContentTypeProfileConfig :: !(Maybe ContentTypeProfileConfig)
  , _flecComment                  :: !(Maybe Text)
  , _flecCallerReference          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flecQueryArgProfileConfig' - A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
--
-- * 'flecContentTypeProfileConfig' - A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
--
-- * 'flecComment' - An optional comment about the configuration.
--
-- * 'flecCallerReference' - A unique number that ensures the request can't be replayed.
fieldLevelEncryptionConfig
    :: Text -- ^ 'flecCallerReference'
    -> FieldLevelEncryptionConfig
fieldLevelEncryptionConfig pCallerReference_ =
  FieldLevelEncryptionConfig'
    { _flecQueryArgProfileConfig = Nothing
    , _flecContentTypeProfileConfig = Nothing
    , _flecComment = Nothing
    , _flecCallerReference = pCallerReference_
    }


-- | A complex data type that specifies when to forward content if a profile isn't found and the profile that can be provided as a query argument in a request.
flecQueryArgProfileConfig :: Lens' FieldLevelEncryptionConfig (Maybe QueryArgProfileConfig)
flecQueryArgProfileConfig = lens _flecQueryArgProfileConfig (\ s a -> s{_flecQueryArgProfileConfig = a})

-- | A complex data type that specifies when to forward content if a content type isn't recognized and profiles to use as by default in a request if a query argument doesn't specify a profile to use.
flecContentTypeProfileConfig :: Lens' FieldLevelEncryptionConfig (Maybe ContentTypeProfileConfig)
flecContentTypeProfileConfig = lens _flecContentTypeProfileConfig (\ s a -> s{_flecContentTypeProfileConfig = a})

-- | An optional comment about the configuration.
flecComment :: Lens' FieldLevelEncryptionConfig (Maybe Text)
flecComment = lens _flecComment (\ s a -> s{_flecComment = a})

-- | A unique number that ensures the request can't be replayed.
flecCallerReference :: Lens' FieldLevelEncryptionConfig Text
flecCallerReference = lens _flecCallerReference (\ s a -> s{_flecCallerReference = a})

instance FromXML FieldLevelEncryptionConfig where
        parseXML x
          = FieldLevelEncryptionConfig' <$>
              (x .@? "QueryArgProfileConfig") <*>
                (x .@? "ContentTypeProfileConfig")
                <*> (x .@? "Comment")
                <*> (x .@ "CallerReference")

instance Hashable FieldLevelEncryptionConfig where

instance NFData FieldLevelEncryptionConfig where

instance ToXML FieldLevelEncryptionConfig where
        toXML FieldLevelEncryptionConfig'{..}
          = mconcat
              ["QueryArgProfileConfig" @=
                 _flecQueryArgProfileConfig,
               "ContentTypeProfileConfig" @=
                 _flecContentTypeProfileConfig,
               "Comment" @= _flecComment,
               "CallerReference" @= _flecCallerReference]

-- | List of field-level encrpytion configurations.
--
--
--
-- /See:/ 'fieldLevelEncryptionList' smart constructor.
data FieldLevelEncryptionList = FieldLevelEncryptionList'
  { _flelItems      :: !(Maybe [FieldLevelEncryptionSummary])
  , _flelNextMarker :: !(Maybe Text)
  , _flelMaxItems   :: !Int
  , _flelQuantity   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flelItems' - An array of field-level encryption items.
--
-- * 'flelNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
--
-- * 'flelMaxItems' - The maximum number of elements you want in the response body.
--
-- * 'flelQuantity' - The number of field-level encryption items.
fieldLevelEncryptionList
    :: Int -- ^ 'flelMaxItems'
    -> Int -- ^ 'flelQuantity'
    -> FieldLevelEncryptionList
fieldLevelEncryptionList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionList'
    { _flelItems = Nothing
    , _flelNextMarker = Nothing
    , _flelMaxItems = pMaxItems_
    , _flelQuantity = pQuantity_
    }


-- | An array of field-level encryption items.
flelItems :: Lens' FieldLevelEncryptionList [FieldLevelEncryptionSummary]
flelItems = lens _flelItems (\ s a -> s{_flelItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
flelNextMarker :: Lens' FieldLevelEncryptionList (Maybe Text)
flelNextMarker = lens _flelNextMarker (\ s a -> s{_flelNextMarker = a})

-- | The maximum number of elements you want in the response body.
flelMaxItems :: Lens' FieldLevelEncryptionList Int
flelMaxItems = lens _flelMaxItems (\ s a -> s{_flelMaxItems = a})

-- | The number of field-level encryption items.
flelQuantity :: Lens' FieldLevelEncryptionList Int
flelQuantity = lens _flelQuantity (\ s a -> s{_flelQuantity = a})

instance FromXML FieldLevelEncryptionList where
        parseXML x
          = FieldLevelEncryptionList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "FieldLevelEncryptionSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "Quantity")

instance Hashable FieldLevelEncryptionList where

instance NFData FieldLevelEncryptionList where

-- | A complex data type for field-level encryption profiles.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { _flepId                                :: !Text
  , _flepLastModifiedTime                  :: !ISO8601
  , _flepFieldLevelEncryptionProfileConfig :: !FieldLevelEncryptionProfileConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepId' - The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
--
-- * 'flepLastModifiedTime' - The last time the field-level encryption profile was updated.
--
-- * 'flepFieldLevelEncryptionProfileConfig' - A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
fieldLevelEncryptionProfile
    :: Text -- ^ 'flepId'
    -> UTCTime -- ^ 'flepLastModifiedTime'
    -> FieldLevelEncryptionProfileConfig -- ^ 'flepFieldLevelEncryptionProfileConfig'
    -> FieldLevelEncryptionProfile
fieldLevelEncryptionProfile pId_ pLastModifiedTime_ pFieldLevelEncryptionProfileConfig_ =
  FieldLevelEncryptionProfile'
    { _flepId = pId_
    , _flepLastModifiedTime = _Time # pLastModifiedTime_
    , _flepFieldLevelEncryptionProfileConfig =
        pFieldLevelEncryptionProfileConfig_
    }


-- | The ID for a field-level encryption profile configuration which includes a set of profiles that specify certain selected data fields to be encrypted by specific public keys.
flepId :: Lens' FieldLevelEncryptionProfile Text
flepId = lens _flepId (\ s a -> s{_flepId = a})

-- | The last time the field-level encryption profile was updated.
flepLastModifiedTime :: Lens' FieldLevelEncryptionProfile UTCTime
flepLastModifiedTime = lens _flepLastModifiedTime (\ s a -> s{_flepLastModifiedTime = a}) . _Time

-- | A complex data type that includes the profile name and the encryption entities for the field-level encryption profile.
flepFieldLevelEncryptionProfileConfig :: Lens' FieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
flepFieldLevelEncryptionProfileConfig = lens _flepFieldLevelEncryptionProfileConfig (\ s a -> s{_flepFieldLevelEncryptionProfileConfig = a})

instance FromXML FieldLevelEncryptionProfile where
        parseXML x
          = FieldLevelEncryptionProfile' <$>
              (x .@ "Id") <*> (x .@ "LastModifiedTime") <*>
                (x .@ "FieldLevelEncryptionProfileConfig")

instance Hashable FieldLevelEncryptionProfile where

instance NFData FieldLevelEncryptionProfile where

-- | A complex data type of profiles for the field-level encryption.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { _flepcComment            :: !(Maybe Text)
  , _flepcName               :: !Text
  , _flepcCallerReference    :: !Text
  , _flepcEncryptionEntities :: !EncryptionEntities
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepcComment' - An optional comment for the field-level encryption profile.
--
-- * 'flepcName' - Profile name for the field-level encryption profile.
--
-- * 'flepcCallerReference' - A unique number that ensures the request can't be replayed.
--
-- * 'flepcEncryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileConfig
    :: Text -- ^ 'flepcName'
    -> Text -- ^ 'flepcCallerReference'
    -> EncryptionEntities -- ^ 'flepcEncryptionEntities'
    -> FieldLevelEncryptionProfileConfig
fieldLevelEncryptionProfileConfig pName_ pCallerReference_ pEncryptionEntities_ =
  FieldLevelEncryptionProfileConfig'
    { _flepcComment = Nothing
    , _flepcName = pName_
    , _flepcCallerReference = pCallerReference_
    , _flepcEncryptionEntities = pEncryptionEntities_
    }


-- | An optional comment for the field-level encryption profile.
flepcComment :: Lens' FieldLevelEncryptionProfileConfig (Maybe Text)
flepcComment = lens _flepcComment (\ s a -> s{_flepcComment = a})

-- | Profile name for the field-level encryption profile.
flepcName :: Lens' FieldLevelEncryptionProfileConfig Text
flepcName = lens _flepcName (\ s a -> s{_flepcName = a})

-- | A unique number that ensures the request can't be replayed.
flepcCallerReference :: Lens' FieldLevelEncryptionProfileConfig Text
flepcCallerReference = lens _flepcCallerReference (\ s a -> s{_flepcCallerReference = a})

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
flepcEncryptionEntities :: Lens' FieldLevelEncryptionProfileConfig EncryptionEntities
flepcEncryptionEntities = lens _flepcEncryptionEntities (\ s a -> s{_flepcEncryptionEntities = a})

instance FromXML FieldLevelEncryptionProfileConfig
         where
        parseXML x
          = FieldLevelEncryptionProfileConfig' <$>
              (x .@? "Comment") <*> (x .@ "Name") <*>
                (x .@ "CallerReference")
                <*> (x .@ "EncryptionEntities")

instance Hashable FieldLevelEncryptionProfileConfig
         where

instance NFData FieldLevelEncryptionProfileConfig
         where

instance ToXML FieldLevelEncryptionProfileConfig
         where
        toXML FieldLevelEncryptionProfileConfig'{..}
          = mconcat
              ["Comment" @= _flepcComment, "Name" @= _flepcName,
               "CallerReference" @= _flepcCallerReference,
               "EncryptionEntities" @= _flepcEncryptionEntities]

-- | List of field-level encryption profiles.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileList' smart constructor.
data FieldLevelEncryptionProfileList = FieldLevelEncryptionProfileList'
  { _fleplItems      :: !(Maybe [FieldLevelEncryptionProfileSummary])
  , _fleplNextMarker :: !(Maybe Text)
  , _fleplMaxItems   :: !Int
  , _fleplQuantity   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionProfileList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleplItems' - The field-level encryption profile items.
--
-- * 'fleplNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
--
-- * 'fleplMaxItems' - The maximum number of field-level encryption profiles you want in the response body.
--
-- * 'fleplQuantity' - The number of field-level encryption profiles.
fieldLevelEncryptionProfileList
    :: Int -- ^ 'fleplMaxItems'
    -> Int -- ^ 'fleplQuantity'
    -> FieldLevelEncryptionProfileList
fieldLevelEncryptionProfileList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionProfileList'
    { _fleplItems = Nothing
    , _fleplNextMarker = Nothing
    , _fleplMaxItems = pMaxItems_
    , _fleplQuantity = pQuantity_
    }


-- | The field-level encryption profile items.
fleplItems :: Lens' FieldLevelEncryptionProfileList [FieldLevelEncryptionProfileSummary]
fleplItems = lens _fleplItems (\ s a -> s{_fleplItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
fleplNextMarker :: Lens' FieldLevelEncryptionProfileList (Maybe Text)
fleplNextMarker = lens _fleplNextMarker (\ s a -> s{_fleplNextMarker = a})

-- | The maximum number of field-level encryption profiles you want in the response body.
fleplMaxItems :: Lens' FieldLevelEncryptionProfileList Int
fleplMaxItems = lens _fleplMaxItems (\ s a -> s{_fleplMaxItems = a})

-- | The number of field-level encryption profiles.
fleplQuantity :: Lens' FieldLevelEncryptionProfileList Int
fleplQuantity = lens _fleplQuantity (\ s a -> s{_fleplQuantity = a})

instance FromXML FieldLevelEncryptionProfileList
         where
        parseXML x
          = FieldLevelEncryptionProfileList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may
                   (parseXMLList "FieldLevelEncryptionProfileSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "Quantity")

instance Hashable FieldLevelEncryptionProfileList
         where

instance NFData FieldLevelEncryptionProfileList where

-- | The field-level encryption profile summary.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileSummary' smart constructor.
data FieldLevelEncryptionProfileSummary = FieldLevelEncryptionProfileSummary'
  { _flepsComment            :: !(Maybe Text)
  , _flepsId                 :: !Text
  , _flepsLastModifiedTime   :: !ISO8601
  , _flepsName               :: !Text
  , _flepsEncryptionEntities :: !EncryptionEntities
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flepsComment' - An optional comment for the field-level encryption profile summary.
--
-- * 'flepsId' - ID for the field-level encryption profile summary.
--
-- * 'flepsLastModifiedTime' - The time when the the field-level encryption profile summary was last updated.
--
-- * 'flepsName' - Name for the field-level encryption profile summary.
--
-- * 'flepsEncryptionEntities' - A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileSummary
    :: Text -- ^ 'flepsId'
    -> UTCTime -- ^ 'flepsLastModifiedTime'
    -> Text -- ^ 'flepsName'
    -> EncryptionEntities -- ^ 'flepsEncryptionEntities'
    -> FieldLevelEncryptionProfileSummary
fieldLevelEncryptionProfileSummary pId_ pLastModifiedTime_ pName_ pEncryptionEntities_ =
  FieldLevelEncryptionProfileSummary'
    { _flepsComment = Nothing
    , _flepsId = pId_
    , _flepsLastModifiedTime = _Time # pLastModifiedTime_
    , _flepsName = pName_
    , _flepsEncryptionEntities = pEncryptionEntities_
    }


-- | An optional comment for the field-level encryption profile summary.
flepsComment :: Lens' FieldLevelEncryptionProfileSummary (Maybe Text)
flepsComment = lens _flepsComment (\ s a -> s{_flepsComment = a})

-- | ID for the field-level encryption profile summary.
flepsId :: Lens' FieldLevelEncryptionProfileSummary Text
flepsId = lens _flepsId (\ s a -> s{_flepsId = a})

-- | The time when the the field-level encryption profile summary was last updated.
flepsLastModifiedTime :: Lens' FieldLevelEncryptionProfileSummary UTCTime
flepsLastModifiedTime = lens _flepsLastModifiedTime (\ s a -> s{_flepsLastModifiedTime = a}) . _Time

-- | Name for the field-level encryption profile summary.
flepsName :: Lens' FieldLevelEncryptionProfileSummary Text
flepsName = lens _flepsName (\ s a -> s{_flepsName = a})

-- | A complex data type of encryption entities for the field-level encryption profile that include the public key ID, provider, and field patterns for specifying which fields to encrypt with this key.
flepsEncryptionEntities :: Lens' FieldLevelEncryptionProfileSummary EncryptionEntities
flepsEncryptionEntities = lens _flepsEncryptionEntities (\ s a -> s{_flepsEncryptionEntities = a})

instance FromXML FieldLevelEncryptionProfileSummary
         where
        parseXML x
          = FieldLevelEncryptionProfileSummary' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "Name")
                <*> (x .@ "EncryptionEntities")

instance Hashable FieldLevelEncryptionProfileSummary
         where

instance NFData FieldLevelEncryptionProfileSummary
         where

-- | A summary of a field-level encryption item.
--
--
--
-- /See:/ 'fieldLevelEncryptionSummary' smart constructor.
data FieldLevelEncryptionSummary = FieldLevelEncryptionSummary'
  { _flesQueryArgProfileConfig    :: !(Maybe QueryArgProfileConfig)
  , _flesContentTypeProfileConfig :: !(Maybe ContentTypeProfileConfig)
  , _flesComment                  :: !(Maybe Text)
  , _flesId                       :: !Text
  , _flesLastModifiedTime         :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldLevelEncryptionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flesQueryArgProfileConfig' - A summary of a query argument-profile mapping.
--
-- * 'flesContentTypeProfileConfig' - A summary of a content type-profile mapping.
--
-- * 'flesComment' - An optional comment about the field-level encryption item.
--
-- * 'flesId' - The unique ID of a field-level encryption item.
--
-- * 'flesLastModifiedTime' - The last time that the summary of field-level encryption items was modified.
fieldLevelEncryptionSummary
    :: Text -- ^ 'flesId'
    -> UTCTime -- ^ 'flesLastModifiedTime'
    -> FieldLevelEncryptionSummary
fieldLevelEncryptionSummary pId_ pLastModifiedTime_ =
  FieldLevelEncryptionSummary'
    { _flesQueryArgProfileConfig = Nothing
    , _flesContentTypeProfileConfig = Nothing
    , _flesComment = Nothing
    , _flesId = pId_
    , _flesLastModifiedTime = _Time # pLastModifiedTime_
    }


-- | A summary of a query argument-profile mapping.
flesQueryArgProfileConfig :: Lens' FieldLevelEncryptionSummary (Maybe QueryArgProfileConfig)
flesQueryArgProfileConfig = lens _flesQueryArgProfileConfig (\ s a -> s{_flesQueryArgProfileConfig = a})

-- | A summary of a content type-profile mapping.
flesContentTypeProfileConfig :: Lens' FieldLevelEncryptionSummary (Maybe ContentTypeProfileConfig)
flesContentTypeProfileConfig = lens _flesContentTypeProfileConfig (\ s a -> s{_flesContentTypeProfileConfig = a})

-- | An optional comment about the field-level encryption item.
flesComment :: Lens' FieldLevelEncryptionSummary (Maybe Text)
flesComment = lens _flesComment (\ s a -> s{_flesComment = a})

-- | The unique ID of a field-level encryption item.
flesId :: Lens' FieldLevelEncryptionSummary Text
flesId = lens _flesId (\ s a -> s{_flesId = a})

-- | The last time that the summary of field-level encryption items was modified.
flesLastModifiedTime :: Lens' FieldLevelEncryptionSummary UTCTime
flesLastModifiedTime = lens _flesLastModifiedTime (\ s a -> s{_flesLastModifiedTime = a}) . _Time

instance FromXML FieldLevelEncryptionSummary where
        parseXML x
          = FieldLevelEncryptionSummary' <$>
              (x .@? "QueryArgProfileConfig") <*>
                (x .@? "ContentTypeProfileConfig")
                <*> (x .@? "Comment")
                <*> (x .@ "Id")
                <*> (x .@ "LastModifiedTime")

instance Hashable FieldLevelEncryptionSummary where

instance NFData FieldLevelEncryptionSummary where

-- | A complex data type that includes the field patterns to match for field-level encryption.
--
--
--
-- /See:/ 'fieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { _fpItems    :: !(Maybe [Text])
  , _fpQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FieldPatterns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fpItems' - An array of the field-level encryption field patterns.
--
-- * 'fpQuantity' - The number of field-level encryption field patterns.
fieldPatterns
    :: Int -- ^ 'fpQuantity'
    -> FieldPatterns
fieldPatterns pQuantity_ =
  FieldPatterns' {_fpItems = Nothing, _fpQuantity = pQuantity_}


-- | An array of the field-level encryption field patterns.
fpItems :: Lens' FieldPatterns [Text]
fpItems = lens _fpItems (\ s a -> s{_fpItems = a}) . _Default . _Coerce

-- | The number of field-level encryption field patterns.
fpQuantity :: Lens' FieldPatterns Int
fpQuantity = lens _fpQuantity (\ s a -> s{_fpQuantity = a})

instance FromXML FieldPatterns where
        parseXML x
          = FieldPatterns' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "FieldPattern"))
                <*> (x .@ "Quantity")

instance Hashable FieldPatterns where

instance NFData FieldPatterns where

instance ToXML FieldPatterns where
        toXML FieldPatterns'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "FieldPattern" <$> _fpItems),
               "Quantity" @= _fpQuantity]

-- | A complex type that specifies how CloudFront handles query strings and cookies.
--
--
--
-- /See:/ 'forwardedValues' smart constructor.
data ForwardedValues = ForwardedValues'
  { _fvQueryStringCacheKeys :: !(Maybe QueryStringCacheKeys)
  , _fvHeaders              :: !(Maybe Headers)
  , _fvQueryString          :: !Bool
  , _fvCookies              :: !CookiePreference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ForwardedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fvQueryStringCacheKeys' - A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
--
-- * 'fvHeaders' - A complex type that specifies the @Headers@ , if any, that you want CloudFront to base caching on for this cache behavior.
--
-- * 'fvQueryString' - Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any: If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin. If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify. If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'fvCookies' - A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
forwardedValues
    :: Bool -- ^ 'fvQueryString'
    -> CookiePreference -- ^ 'fvCookies'
    -> ForwardedValues
forwardedValues pQueryString_ pCookies_ =
  ForwardedValues'
    { _fvQueryStringCacheKeys = Nothing
    , _fvHeaders = Nothing
    , _fvQueryString = pQueryString_
    , _fvCookies = pCookies_
    }


-- | A complex type that contains information about the query string parameters that you want CloudFront to use for caching for this cache behavior.
fvQueryStringCacheKeys :: Lens' ForwardedValues (Maybe QueryStringCacheKeys)
fvQueryStringCacheKeys = lens _fvQueryStringCacheKeys (\ s a -> s{_fvQueryStringCacheKeys = a})

-- | A complex type that specifies the @Headers@ , if any, that you want CloudFront to base caching on for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\ s a -> s{_fvHeaders = a})

-- | Indicates whether you want CloudFront to forward query strings to the origin that is associated with this cache behavior and cache based on the query string parameters. CloudFront behavior depends on the value of @QueryString@ and on the values that you specify for @QueryStringCacheKeys@ , if any: If you specify true for @QueryString@ and you don't specify any values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin and caches based on all query string parameters. Depending on how many query string parameters and values you have, this can adversely affect performance because CloudFront must forward more requests to the origin. If you specify true for @QueryString@ and you specify one or more values for @QueryStringCacheKeys@ , CloudFront forwards all query string parameters to the origin, but it only caches based on the query string parameters that you specify. If you specify false for @QueryString@ , CloudFront doesn't forward any query string parameters to the origin, and doesn't cache based on query string parameters. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/QueryStringParameters.html Configuring CloudFront to Cache Based on Query String Parameters> in the /Amazon CloudFront Developer Guide/ .
fvQueryString :: Lens' ForwardedValues Bool
fvQueryString = lens _fvQueryString (\ s a -> s{_fvQueryString = a})

-- | A complex type that specifies whether you want CloudFront to forward cookies to the origin and, if so, which ones. For more information about forwarding cookies to the origin, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html How CloudFront Forwards, Caches, and Logs Cookies> in the /Amazon CloudFront Developer Guide/ .
fvCookies :: Lens' ForwardedValues CookiePreference
fvCookies = lens _fvCookies (\ s a -> s{_fvCookies = a})

instance FromXML ForwardedValues where
        parseXML x
          = ForwardedValues' <$>
              (x .@? "QueryStringCacheKeys") <*> (x .@? "Headers")
                <*> (x .@ "QueryString")
                <*> (x .@ "Cookies")

instance Hashable ForwardedValues where

instance NFData ForwardedValues where

instance ToXML ForwardedValues where
        toXML ForwardedValues'{..}
          = mconcat
              ["QueryStringCacheKeys" @= _fvQueryStringCacheKeys,
               "Headers" @= _fvHeaders,
               "QueryString" @= _fvQueryString,
               "Cookies" @= _fvCookies]

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
--
--
--
-- /See:/ 'geoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
  { _grItems           :: !(Maybe [Text])
  , _grRestrictionType :: !GeoRestrictionType
  , _grQuantity        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GeoRestriction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grItems' - A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ). The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country. CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
--
-- * 'grRestrictionType' - The method that you want to use to restrict distribution of your content by country:     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
--
-- * 'grQuantity' - When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
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


-- | A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ). The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country. CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
grItems :: Lens' GeoRestriction [Text]
grItems = lens _grItems (\ s a -> s{_grItems = a}) . _Default . _Coerce

-- | The method that you want to use to restrict distribution of your content by country:     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
grRestrictionType :: Lens' GeoRestriction GeoRestrictionType
grRestrictionType = lens _grRestrictionType (\ s a -> s{_grRestrictionType = a})

-- | When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
grQuantity :: Lens' GeoRestriction Int
grQuantity = lens _grQuantity (\ s a -> s{_grQuantity = a})

instance FromXML GeoRestriction where
        parseXML x
          = GeoRestriction' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Location"))
                <*> (x .@ "RestrictionType")
                <*> (x .@ "Quantity")

instance Hashable GeoRestriction where

instance NFData GeoRestriction where

instance ToXML GeoRestriction where
        toXML GeoRestriction'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "Location" <$> _grItems),
               "RestrictionType" @= _grRestrictionType,
               "Quantity" @= _grQuantity]

-- | A complex type that specifies the request headers, if any, that you want CloudFront to base caching on for this cache behavior.
--
--
-- For the headers that you specify, CloudFront caches separate versions of a specified object based on the header values in viewer requests. For example, suppose viewer requests for @logo.jpg@ contain a custom @product@ header that has a value of either @acme@ or @apex@ , and you configure CloudFront to cache your content based on values in the @product@ header. CloudFront forwards the @product@ header to the origin and caches the response from the origin once for each header value. For more information about caching based on header values, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/header-caching.html How CloudFront Forwards and Caches Headers> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'headers' smart constructor.
data Headers = Headers'
  { _hItems    :: !(Maybe [Text])
  , _hQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hItems' - A list that contains one @Name@ element for each header that you want CloudFront to use for caching in this cache behavior. If @Quantity@ is @0@ , omit @Items@ .
--
-- * 'hQuantity' - The number of different headers that you want CloudFront to base caching on for this cache behavior. You can configure each cache behavior in a web distribution to do one of the following:     * __Forward all headers to your origin__ : Specify @1@ for @Quantity@ and @*@ for @Name@ . /Important:/ CloudFront doesn't cache the objects that are associated with this cache behavior. Instead, CloudFront sends every request to the origin.      * __Forward a whitelist of headers you specify__ : Specify the number of headers that you want CloudFront to base caching on. Then specify the header names in @Name@ elements. CloudFront caches your objects based on the values in the specified headers.     * __Forward only the default headers__ : Specify @0@ for @Quantity@ and omit @Items@ . In this configuration, CloudFront doesn't cache based on the values in the request headers. Regardless of which option you choose, CloudFront forwards headers to your origin based on whether the origin is an S3 bucket or a custom origin. See the following documentation:     * __S3 bucket__ : See <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorS3Origin.html#request-s3-removed-headers HTTP Request Headers That CloudFront Removes or Updates>      * __Custom origin__ : See <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorCustomOrigin.html#request-custom-headers-behavior HTTP Request Headers and CloudFront Behavior>
headers
    :: Int -- ^ 'hQuantity'
    -> Headers
headers pQuantity_ = Headers' {_hItems = Nothing, _hQuantity = pQuantity_}


-- | A list that contains one @Name@ element for each header that you want CloudFront to use for caching in this cache behavior. If @Quantity@ is @0@ , omit @Items@ .
hItems :: Lens' Headers [Text]
hItems = lens _hItems (\ s a -> s{_hItems = a}) . _Default . _Coerce

-- | The number of different headers that you want CloudFront to base caching on for this cache behavior. You can configure each cache behavior in a web distribution to do one of the following:     * __Forward all headers to your origin__ : Specify @1@ for @Quantity@ and @*@ for @Name@ . /Important:/ CloudFront doesn't cache the objects that are associated with this cache behavior. Instead, CloudFront sends every request to the origin.      * __Forward a whitelist of headers you specify__ : Specify the number of headers that you want CloudFront to base caching on. Then specify the header names in @Name@ elements. CloudFront caches your objects based on the values in the specified headers.     * __Forward only the default headers__ : Specify @0@ for @Quantity@ and omit @Items@ . In this configuration, CloudFront doesn't cache based on the values in the request headers. Regardless of which option you choose, CloudFront forwards headers to your origin based on whether the origin is an S3 bucket or a custom origin. See the following documentation:     * __S3 bucket__ : See <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorS3Origin.html#request-s3-removed-headers HTTP Request Headers That CloudFront Removes or Updates>      * __Custom origin__ : See <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/RequestAndResponseBehaviorCustomOrigin.html#request-custom-headers-behavior HTTP Request Headers and CloudFront Behavior>
hQuantity :: Lens' Headers Int
hQuantity = lens _hQuantity (\ s a -> s{_hQuantity = a})

instance FromXML Headers where
        parseXML x
          = Headers' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance Hashable Headers where

instance NFData Headers where

instance ToXML Headers where
        toXML Headers'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _hItems),
               "Quantity" @= _hQuantity]

-- | An invalidation.
--
--
--
-- /See:/ 'invalidation' smart constructor.
data Invalidation = Invalidation'
  { _iId                :: !Text
  , _iStatus            :: !Text
  , _iCreateTime        :: !ISO8601
  , _iInvalidationBatch :: !InvalidationBatch
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Invalidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId' - The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
--
-- * 'iStatus' - The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
--
-- * 'iCreateTime' - The date and time the invalidation request was first made.
--
-- * 'iInvalidationBatch' - The current invalidation information for the batch request.
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


-- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
iId :: Lens' Invalidation Text
iId = lens _iId (\ s a -> s{_iId = a})

-- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
iStatus :: Lens' Invalidation Text
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | The date and time the invalidation request was first made.
iCreateTime :: Lens' Invalidation UTCTime
iCreateTime = lens _iCreateTime (\ s a -> s{_iCreateTime = a}) . _Time

-- | The current invalidation information for the batch request.
iInvalidationBatch :: Lens' Invalidation InvalidationBatch
iInvalidationBatch = lens _iInvalidationBatch (\ s a -> s{_iInvalidationBatch = a})

instance FromXML Invalidation where
        parseXML x
          = Invalidation' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "CreateTime")
                <*> (x .@ "InvalidationBatch")

instance Hashable Invalidation where

instance NFData Invalidation where

-- | An invalidation batch.
--
--
--
-- /See:/ 'invalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
  { _ibPaths           :: !Paths
  , _ibCallerReference :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvalidationBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibPaths' - A complex type that contains information about the objects that you want to invalidate. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'ibCallerReference' - A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ . If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ . If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
invalidationBatch
    :: Paths -- ^ 'ibPaths'
    -> Text -- ^ 'ibCallerReference'
    -> InvalidationBatch
invalidationBatch pPaths_ pCallerReference_ =
  InvalidationBatch'
    {_ibPaths = pPaths_, _ibCallerReference = pCallerReference_}


-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
ibPaths :: Lens' InvalidationBatch Paths
ibPaths = lens _ibPaths (\ s a -> s{_ibPaths = a})

-- | A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ . If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ . If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
ibCallerReference :: Lens' InvalidationBatch Text
ibCallerReference = lens _ibCallerReference (\ s a -> s{_ibCallerReference = a})

instance FromXML InvalidationBatch where
        parseXML x
          = InvalidationBatch' <$>
              (x .@ "Paths") <*> (x .@ "CallerReference")

instance Hashable InvalidationBatch where

instance NFData InvalidationBatch where

instance ToXML InvalidationBatch where
        toXML InvalidationBatch'{..}
          = mconcat
              ["Paths" @= _ibPaths,
               "CallerReference" @= _ibCallerReference]

-- | The @InvalidationList@ complex type describes the list of invalidation objects. For more information about invalidation, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html Invalidating Objects (Web Distributions Only)> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'invalidationList' smart constructor.
data InvalidationList = InvalidationList'
  { _ilItems       :: !(Maybe [InvalidationSummary])
  , _ilNextMarker  :: !(Maybe Text)
  , _ilMarker      :: !Text
  , _ilMaxItems    :: !Int
  , _ilIsTruncated :: !Bool
  , _ilQuantity    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvalidationList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilItems' - A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
--
-- * 'ilNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
--
-- * 'ilMarker' - The value that you provided for the @Marker@ request parameter.
--
-- * 'ilMaxItems' - The value that you provided for the @MaxItems@ request parameter.
--
-- * 'ilIsTruncated' - A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
--
-- * 'ilQuantity' - The number of invalidation batches that were created by the current AWS account.
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


-- | A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
ilItems :: Lens' InvalidationList [InvalidationSummary]
ilItems = lens _ilItems (\ s a -> s{_ilItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\ s a -> s{_ilNextMarker = a})

-- | The value that you provided for the @Marker@ request parameter.
ilMarker :: Lens' InvalidationList Text
ilMarker = lens _ilMarker (\ s a -> s{_ilMarker = a})

-- | The value that you provided for the @MaxItems@ request parameter.
ilMaxItems :: Lens' InvalidationList Int
ilMaxItems = lens _ilMaxItems (\ s a -> s{_ilMaxItems = a})

-- | A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
ilIsTruncated :: Lens' InvalidationList Bool
ilIsTruncated = lens _ilIsTruncated (\ s a -> s{_ilIsTruncated = a})

-- | The number of invalidation batches that were created by the current AWS account.
ilQuantity :: Lens' InvalidationList Int
ilQuantity = lens _ilQuantity (\ s a -> s{_ilQuantity = a})

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

instance Hashable InvalidationList where

instance NFData InvalidationList where

-- | A summary of an invalidation request.
--
--
--
-- /See:/ 'invalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
  { _isId         :: !Text
  , _isCreateTime :: !ISO8601
  , _isStatus     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InvalidationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isId' - The unique ID for an invalidation request.
--
-- * 'isCreateTime' - Undocumented member.
--
-- * 'isStatus' - The status of an invalidation request.
invalidationSummary
    :: Text -- ^ 'isId'
    -> UTCTime -- ^ 'isCreateTime'
    -> Text -- ^ 'isStatus'
    -> InvalidationSummary
invalidationSummary pId_ pCreateTime_ pStatus_ =
  InvalidationSummary'
    {_isId = pId_, _isCreateTime = _Time # pCreateTime_, _isStatus = pStatus_}


-- | The unique ID for an invalidation request.
isId :: Lens' InvalidationSummary Text
isId = lens _isId (\ s a -> s{_isId = a})

-- | Undocumented member.
isCreateTime :: Lens' InvalidationSummary UTCTime
isCreateTime = lens _isCreateTime (\ s a -> s{_isCreateTime = a}) . _Time

-- | The status of an invalidation request.
isStatus :: Lens' InvalidationSummary Text
isStatus = lens _isStatus (\ s a -> s{_isStatus = a})

instance FromXML InvalidationSummary where
        parseXML x
          = InvalidationSummary' <$>
              (x .@ "Id") <*> (x .@ "CreateTime") <*>
                (x .@ "Status")

instance Hashable InvalidationSummary where

instance NFData InvalidationSummary where

-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ .
--
--
-- For more information, see 'ActiveTrustedSigners' .
--
--
-- /See:/ 'keyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { _kpiItems    :: !(Maybe [Text])
  , _kpiQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyPairIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiItems' - A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ . For more information, see 'ActiveTrustedSigners' .
--
-- * 'kpiQuantity' - The number of active CloudFront key pairs for @AwsAccountNumber@ . For more information, see 'ActiveTrustedSigners' .
keyPairIds
    :: Int -- ^ 'kpiQuantity'
    -> KeyPairIds
keyPairIds pQuantity_ =
  KeyPairIds' {_kpiItems = Nothing, _kpiQuantity = pQuantity_}


-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ . For more information, see 'ActiveTrustedSigners' .
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\ s a -> s{_kpiItems = a}) . _Default . _Coerce

-- | The number of active CloudFront key pairs for @AwsAccountNumber@ . For more information, see 'ActiveTrustedSigners' .
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\ s a -> s{_kpiQuantity = a})

instance FromXML KeyPairIds where
        parseXML x
          = KeyPairIds' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "KeyPairId"))
                <*> (x .@ "Quantity")

instance Hashable KeyPairIds where

instance NFData KeyPairIds where

-- | A complex type that contains a Lambda function association.
--
--
--
-- /See:/ 'lambdaFunctionAssociation' smart constructor.
data LambdaFunctionAssociation = LambdaFunctionAssociation'
  { _lfaLambdaFunctionARN :: !Text
  , _lfaEventType         :: !EventType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfaLambdaFunctionARN' - The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
--
-- * 'lfaEventType' - Specifies the event type that triggers a Lambda function invocation. You can specify the following values:     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.      * @origin-request@ : The function executes only when CloudFront forwards a request to your origin. When the requested object is in the edge cache, the function doesn't execute.     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
lambdaFunctionAssociation
    :: Text -- ^ 'lfaLambdaFunctionARN'
    -> EventType -- ^ 'lfaEventType'
    -> LambdaFunctionAssociation
lambdaFunctionAssociation pLambdaFunctionARN_ pEventType_ =
  LambdaFunctionAssociation'
    {_lfaLambdaFunctionARN = pLambdaFunctionARN_, _lfaEventType = pEventType_}


-- | The ARN of the Lambda function. You must specify the ARN of a function version; you can't specify a Lambda alias or $LATEST.
lfaLambdaFunctionARN :: Lens' LambdaFunctionAssociation Text
lfaLambdaFunctionARN = lens _lfaLambdaFunctionARN (\ s a -> s{_lfaLambdaFunctionARN = a})

-- | Specifies the event type that triggers a Lambda function invocation. You can specify the following values:     * @viewer-request@ : The function executes when CloudFront receives a request from a viewer and before it checks to see whether the requested object is in the edge cache.      * @origin-request@ : The function executes only when CloudFront forwards a request to your origin. When the requested object is in the edge cache, the function doesn't execute.     * @origin-response@ : The function executes after CloudFront receives a response from the origin and before it caches the object in the response. When the requested object is in the edge cache, the function doesn't execute. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.     * @viewer-response@ : The function executes before CloudFront returns the requested object to the viewer. The function executes regardless of whether the object was already in the edge cache. If the origin returns an HTTP status code other than HTTP 200 (OK), the function doesn't execute.
lfaEventType :: Lens' LambdaFunctionAssociation EventType
lfaEventType = lens _lfaEventType (\ s a -> s{_lfaEventType = a})

instance FromXML LambdaFunctionAssociation where
        parseXML x
          = LambdaFunctionAssociation' <$>
              (x .@ "LambdaFunctionARN") <*> (x .@ "EventType")

instance Hashable LambdaFunctionAssociation where

instance NFData LambdaFunctionAssociation where

instance ToXML LambdaFunctionAssociation where
        toXML LambdaFunctionAssociation'{..}
          = mconcat
              ["LambdaFunctionARN" @= _lfaLambdaFunctionARN,
               "EventType" @= _lfaEventType]

-- | A complex type that specifies a list of Lambda functions associations for a cache behavior.
--
--
-- If you want to invoke one or more Lambda functions triggered by requests that match the @PathPattern@ of the cache behavior, specify the applicable values for @Quantity@ and @Items@ . Note that there can be up to 4 @LambdaFunctionAssociation@ items in this list (one for each possible value of @EventType@ ) and each @EventType@ can be associated with the Lambda function only once.
--
-- If you don't want to invoke any Lambda functions for the requests that match @PathPattern@ , specify @0@ for @Quantity@ and omit @Items@ .
--
--
-- /See:/ 'lambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { _lfaItems    :: !(Maybe [LambdaFunctionAssociation])
  , _lfaQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LambdaFunctionAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfaItems' - __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'lfaQuantity' - The number of Lambda function associations for this cache behavior.
lambdaFunctionAssociations
    :: Int -- ^ 'lfaQuantity'
    -> LambdaFunctionAssociations
lambdaFunctionAssociations pQuantity_ =
  LambdaFunctionAssociations' {_lfaItems = Nothing, _lfaQuantity = pQuantity_}


-- | __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
lfaItems :: Lens' LambdaFunctionAssociations [LambdaFunctionAssociation]
lfaItems = lens _lfaItems (\ s a -> s{_lfaItems = a}) . _Default . _Coerce

-- | The number of Lambda function associations for this cache behavior.
lfaQuantity :: Lens' LambdaFunctionAssociations Int
lfaQuantity = lens _lfaQuantity (\ s a -> s{_lfaQuantity = a})

instance FromXML LambdaFunctionAssociations where
        parseXML x
          = LambdaFunctionAssociations' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "LambdaFunctionAssociation"))
                <*> (x .@ "Quantity")

instance Hashable LambdaFunctionAssociations where

instance NFData LambdaFunctionAssociations where

instance ToXML LambdaFunctionAssociations where
        toXML LambdaFunctionAssociations'{..}
          = mconcat
              ["Items" @=
                 toXML
                   (toXMLList "LambdaFunctionAssociation" <$>
                      _lfaItems),
               "Quantity" @= _lfaQuantity]

-- | A complex type that controls whether access logs are written for the distribution.
--
--
--
-- /See:/ 'loggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { _lcEnabled        :: !Bool
  , _lcIncludeCookies :: !Bool
  , _lcBucket         :: !Text
  , _lcPrefix         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcEnabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
--
-- * 'lcIncludeCookies' - Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
--
-- * 'lcBucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- * 'lcPrefix' - An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
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


-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a distribution or if you want to disable logging for an existing distribution, specify @false@ for @Enabled@ , and specify empty @Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ , @prefix@ , and @IncludeCookies@ , the values are automatically deleted.
lcEnabled :: Lens' LoggingConfig Bool
lcEnabled = lens _lcEnabled (\ s a -> s{_lcEnabled = a})

-- | Specifies whether you want CloudFront to include cookies in access logs, specify @true@ for @IncludeCookies@ . If you choose to include cookies in logs, CloudFront logs all cookies regardless of how you configure the cache behaviors for this distribution. If you don't want to include cookies when you create a distribution or if you want to disable include cookies for an existing distribution, specify @false@ for @IncludeCookies@ .
lcIncludeCookies :: Lens' LoggingConfig Bool
lcIncludeCookies = lens _lcIncludeCookies (\ s a -> s{_lcIncludeCookies = a})

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
lcBucket :: Lens' LoggingConfig Text
lcBucket = lens _lcBucket (\ s a -> s{_lcBucket = a})

-- | An optional string that you want CloudFront to prefix to the access log @filenames@ for this distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
lcPrefix :: Lens' LoggingConfig Text
lcPrefix = lens _lcPrefix (\ s a -> s{_lcPrefix = a})

instance FromXML LoggingConfig where
        parseXML x
          = LoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "IncludeCookies") <*>
                (x .@ "Bucket")
                <*> (x .@ "Prefix")

instance Hashable LoggingConfig where

instance NFData LoggingConfig where

instance ToXML LoggingConfig where
        toXML LoggingConfig'{..}
          = mconcat
              ["Enabled" @= _lcEnabled,
               "IncludeCookies" @= _lcIncludeCookies,
               "Bucket" @= _lcBucket, "Prefix" @= _lcPrefix]

-- | A complex type that describes the Amazon S3 bucket or the HTTP server (for example, a web server) from which CloudFront gets your files. You must create at least one origin.
--
--
-- For the current limit on the number of origins that you can create for a distribution, see <http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_cloudfront Amazon CloudFront Limits> in the /AWS General Reference/ .
--
--
-- /See:/ 'origin' smart constructor.
data Origin = Origin'
  { _oCustomHeaders      :: !(Maybe CustomHeaders)
  , _oCustomOriginConfig :: !(Maybe CustomOriginConfig)
  , _oS3OriginConfig     :: !(Maybe S3OriginConfig)
  , _oOriginPath         :: !(Maybe Text)
  , _oId                 :: !Text
  , _oDomainName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oCustomHeaders' - A complex type that contains names and values for the custom headers that you want.
--
-- * 'oCustomOriginConfig' - A complex type that contains information about a custom origin. If the origin is an Amazon S3 bucket, use the @S3OriginConfig@ element instead.
--
-- * 'oS3OriginConfig' - A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the @CustomOriginConfig@ element instead.
--
-- * 'oOriginPath' - An optional element that causes CloudFront to request your content from a directory in your Amazon S3 bucket or your custom origin. When you include the @OriginPath@ element, specify the directory name, beginning with a @/@ . CloudFront appends the directory name to the value of @DomainName@ , for example, @example.com/production@ . Do not include a @/@ at the end of the directory name. For example, suppose you've specified the following values for your distribution:     * @DomainName@ : An Amazon S3 bucket named @myawsbucket@ .     * @OriginPath@ : @/production@      * @CNAME@ : @example.com@  When a user enters @example.com/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/index.html@ . When a user enters @example.com/acme/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/acme/index.html@ .
--
-- * 'oId' - A unique identifier for the origin. The value of @Id@ must be unique within the distribution. When you specify the value of @TargetOriginId@ for the default cache behavior or for another cache behavior, you indicate the origin to which you want the cache behavior to route requests by specifying the value of the @Id@ element for that origin. When a request matches the path pattern for that cache behavior, CloudFront routes the request to the specified origin. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'oDomainName' - __Amazon S3 origins__ : The DNS name of the Amazon S3 bucket from which you want CloudFront to get objects for this origin, for example, @myawsbucket.s3.amazonaws.com@ . Constraints for Amazon S3 origins:      * If you configured Amazon S3 Transfer Acceleration for your bucket, don't specify the @s3-accelerate@ endpoint for @DomainName@ .     * The bucket name must be between 3 and 63 characters long (inclusive).     * The bucket name must contain only lowercase characters, numbers, periods, underscores, and dashes.     * The bucket name must not contain adjacent periods. __Custom Origins__ : The DNS domain name for the HTTP server from which you want CloudFront to get objects for this origin, for example, @www.example.com@ .  Constraints for custom origins:     * @DomainName@ must be a valid DNS name that contains only a-z, A-Z, 0-9, dot (.), hyphen (-), or underscore (_) characters.     * The name cannot exceed 128 characters.
origin
    :: Text -- ^ 'oId'
    -> Text -- ^ 'oDomainName'
    -> Origin
origin pId_ pDomainName_ =
  Origin'
    { _oCustomHeaders = Nothing
    , _oCustomOriginConfig = Nothing
    , _oS3OriginConfig = Nothing
    , _oOriginPath = Nothing
    , _oId = pId_
    , _oDomainName = pDomainName_
    }


-- | A complex type that contains names and values for the custom headers that you want.
oCustomHeaders :: Lens' Origin (Maybe CustomHeaders)
oCustomHeaders = lens _oCustomHeaders (\ s a -> s{_oCustomHeaders = a})

-- | A complex type that contains information about a custom origin. If the origin is an Amazon S3 bucket, use the @S3OriginConfig@ element instead.
oCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oCustomOriginConfig = lens _oCustomOriginConfig (\ s a -> s{_oCustomOriginConfig = a})

-- | A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the @CustomOriginConfig@ element instead.
oS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oS3OriginConfig = lens _oS3OriginConfig (\ s a -> s{_oS3OriginConfig = a})

-- | An optional element that causes CloudFront to request your content from a directory in your Amazon S3 bucket or your custom origin. When you include the @OriginPath@ element, specify the directory name, beginning with a @/@ . CloudFront appends the directory name to the value of @DomainName@ , for example, @example.com/production@ . Do not include a @/@ at the end of the directory name. For example, suppose you've specified the following values for your distribution:     * @DomainName@ : An Amazon S3 bucket named @myawsbucket@ .     * @OriginPath@ : @/production@      * @CNAME@ : @example.com@  When a user enters @example.com/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/index.html@ . When a user enters @example.com/acme/index.html@ in a browser, CloudFront sends a request to Amazon S3 for @myawsbucket/production/acme/index.html@ .
oOriginPath :: Lens' Origin (Maybe Text)
oOriginPath = lens _oOriginPath (\ s a -> s{_oOriginPath = a})

-- | A unique identifier for the origin. The value of @Id@ must be unique within the distribution. When you specify the value of @TargetOriginId@ for the default cache behavior or for another cache behavior, you indicate the origin to which you want the cache behavior to route requests by specifying the value of the @Id@ element for that origin. When a request matches the path pattern for that cache behavior, CloudFront routes the request to the specified origin. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesCacheBehavior Cache Behavior Settings> in the /Amazon CloudFront Developer Guide/ .
oId :: Lens' Origin Text
oId = lens _oId (\ s a -> s{_oId = a})

-- | __Amazon S3 origins__ : The DNS name of the Amazon S3 bucket from which you want CloudFront to get objects for this origin, for example, @myawsbucket.s3.amazonaws.com@ . Constraints for Amazon S3 origins:      * If you configured Amazon S3 Transfer Acceleration for your bucket, don't specify the @s3-accelerate@ endpoint for @DomainName@ .     * The bucket name must be between 3 and 63 characters long (inclusive).     * The bucket name must contain only lowercase characters, numbers, periods, underscores, and dashes.     * The bucket name must not contain adjacent periods. __Custom Origins__ : The DNS domain name for the HTTP server from which you want CloudFront to get objects for this origin, for example, @www.example.com@ .  Constraints for custom origins:     * @DomainName@ must be a valid DNS name that contains only a-z, A-Z, 0-9, dot (.), hyphen (-), or underscore (_) characters.     * The name cannot exceed 128 characters.
oDomainName :: Lens' Origin Text
oDomainName = lens _oDomainName (\ s a -> s{_oDomainName = a})

instance FromXML Origin where
        parseXML x
          = Origin' <$>
              (x .@? "CustomHeaders") <*>
                (x .@? "CustomOriginConfig")
                <*> (x .@? "S3OriginConfig")
                <*> (x .@? "OriginPath")
                <*> (x .@ "Id")
                <*> (x .@ "DomainName")

instance Hashable Origin where

instance NFData Origin where

instance ToXML Origin where
        toXML Origin'{..}
          = mconcat
              ["CustomHeaders" @= _oCustomHeaders,
               "CustomOriginConfig" @= _oCustomOriginConfig,
               "S3OriginConfig" @= _oS3OriginConfig,
               "OriginPath" @= _oOriginPath, "Id" @= _oId,
               "DomainName" @= _oDomainName]

-- | A complex type that contains @HeaderName@ and @HeaderValue@ elements, if any, for this distribution.
--
--
--
-- /See:/ 'originCustomHeader' smart constructor.
data OriginCustomHeader = OriginCustomHeader'
  { _ochHeaderName  :: !Text
  , _ochHeaderValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OriginCustomHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ochHeaderName' - The name of a header that you want CloudFront to forward to your origin. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Forwarding Custom Headers to Your Origin (Web Distributions Only)> in the /Amazon Amazon CloudFront Developer Guide/ .
--
-- * 'ochHeaderValue' - The value for the header that you specified in the @HeaderName@ field.
originCustomHeader
    :: Text -- ^ 'ochHeaderName'
    -> Text -- ^ 'ochHeaderValue'
    -> OriginCustomHeader
originCustomHeader pHeaderName_ pHeaderValue_ =
  OriginCustomHeader'
    {_ochHeaderName = pHeaderName_, _ochHeaderValue = pHeaderValue_}


-- | The name of a header that you want CloudFront to forward to your origin. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Forwarding Custom Headers to Your Origin (Web Distributions Only)> in the /Amazon Amazon CloudFront Developer Guide/ .
ochHeaderName :: Lens' OriginCustomHeader Text
ochHeaderName = lens _ochHeaderName (\ s a -> s{_ochHeaderName = a})

-- | The value for the header that you specified in the @HeaderName@ field.
ochHeaderValue :: Lens' OriginCustomHeader Text
ochHeaderValue = lens _ochHeaderValue (\ s a -> s{_ochHeaderValue = a})

instance FromXML OriginCustomHeader where
        parseXML x
          = OriginCustomHeader' <$>
              (x .@ "HeaderName") <*> (x .@ "HeaderValue")

instance Hashable OriginCustomHeader where

instance NFData OriginCustomHeader where

instance ToXML OriginCustomHeader where
        toXML OriginCustomHeader'{..}
          = mconcat
              ["HeaderName" @= _ochHeaderName,
               "HeaderValue" @= _ochHeaderValue]

-- | A complex type that contains information about the SSL/TLS protocols that CloudFront can use when establishing an HTTPS connection with your origin.
--
--
--
-- /See:/ 'originSSLProtocols' smart constructor.
data OriginSSLProtocols = OriginSSLProtocols'
  { _ospQuantity :: !Int
  , _ospItems    :: ![SSLProtocol]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OriginSSLProtocols' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ospQuantity' - The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
--
-- * 'ospItems' - A list that contains allowed SSL/TLS protocols for this distribution.
originSSLProtocols
    :: Int -- ^ 'ospQuantity'
    -> OriginSSLProtocols
originSSLProtocols pQuantity_ =
  OriginSSLProtocols' {_ospQuantity = pQuantity_, _ospItems = mempty}


-- | The number of SSL/TLS protocols that you want to allow CloudFront to use when establishing an HTTPS connection with this origin.
ospQuantity :: Lens' OriginSSLProtocols Int
ospQuantity = lens _ospQuantity (\ s a -> s{_ospQuantity = a})

-- | A list that contains allowed SSL/TLS protocols for this distribution.
ospItems :: Lens' OriginSSLProtocols [SSLProtocol]
ospItems = lens _ospItems (\ s a -> s{_ospItems = a}) . _Coerce

instance FromXML OriginSSLProtocols where
        parseXML x
          = OriginSSLProtocols' <$>
              (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>=
                   parseXMLList "SslProtocol")

instance Hashable OriginSSLProtocols where

instance NFData OriginSSLProtocols where

instance ToXML OriginSSLProtocols where
        toXML OriginSSLProtocols'{..}
          = mconcat
              ["Quantity" @= _ospQuantity,
               "Items" @= toXMLList "SslProtocol" _ospItems]

-- | A complex type that contains information about origins for this distribution.
--
--
--
-- /See:/ 'origins' smart constructor.
data Origins = Origins'
  { _oItems    :: !(Maybe (List1 Origin))
  , _oQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Origins' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oItems' - A complex type that contains origins for this distribution.
--
-- * 'oQuantity' - The number of origins for this distribution.
origins
    :: Int -- ^ 'oQuantity'
    -> Origins
origins pQuantity_ = Origins' {_oItems = Nothing, _oQuantity = pQuantity_}


-- | A complex type that contains origins for this distribution.
oItems :: Lens' Origins (Maybe (NonEmpty Origin))
oItems = lens _oItems (\ s a -> s{_oItems = a}) . mapping _List1

-- | The number of origins for this distribution.
oQuantity :: Lens' Origins Int
oQuantity = lens _oQuantity (\ s a -> s{_oQuantity = a})

instance FromXML Origins where
        parseXML x
          = Origins' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList1 "Origin"))
                <*> (x .@ "Quantity")

instance Hashable Origins where

instance NFData Origins where

instance ToXML Origins where
        toXML Origins'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Origin" <$> _oItems),
               "Quantity" @= _oQuantity]

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'paths' smart constructor.
data Paths = Paths'
  { _pItems    :: !(Maybe [Text])
  , _pQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Paths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pItems' - A complex type that contains a list of the paths that you want to invalidate.
--
-- * 'pQuantity' - The number of objects that you want to invalidate.
paths
    :: Int -- ^ 'pQuantity'
    -> Paths
paths pQuantity_ = Paths' {_pItems = Nothing, _pQuantity = pQuantity_}


-- | A complex type that contains a list of the paths that you want to invalidate.
pItems :: Lens' Paths [Text]
pItems = lens _pItems (\ s a -> s{_pItems = a}) . _Default . _Coerce

-- | The number of objects that you want to invalidate.
pQuantity :: Lens' Paths Int
pQuantity = lens _pQuantity (\ s a -> s{_pQuantity = a})

instance FromXML Paths where
        parseXML x
          = Paths' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Path"))
                <*> (x .@ "Quantity")

instance Hashable Paths where

instance NFData Paths where

instance ToXML Paths where
        toXML Paths'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Path" <$> _pItems),
               "Quantity" @= _pQuantity]

-- | A complex data type of public keys you add to CloudFront to use with features like field-level encryption.
--
--
--
-- /See:/ 'publicKey' smart constructor.
data PublicKey = PublicKey'
  { _pkId              :: !Text
  , _pkCreatedTime     :: !ISO8601
  , _pkPublicKeyConfig :: !PublicKeyConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkId' - A unique ID assigned to a public key you've added to CloudFront.
--
-- * 'pkCreatedTime' - A time you added a public key to CloudFront.
--
-- * 'pkPublicKeyConfig' - A complex data type for a public key you add to CloudFront to use with features like field-level encryption.
publicKey
    :: Text -- ^ 'pkId'
    -> UTCTime -- ^ 'pkCreatedTime'
    -> PublicKeyConfig -- ^ 'pkPublicKeyConfig'
    -> PublicKey
publicKey pId_ pCreatedTime_ pPublicKeyConfig_ =
  PublicKey'
    { _pkId = pId_
    , _pkCreatedTime = _Time # pCreatedTime_
    , _pkPublicKeyConfig = pPublicKeyConfig_
    }


-- | A unique ID assigned to a public key you've added to CloudFront.
pkId :: Lens' PublicKey Text
pkId = lens _pkId (\ s a -> s{_pkId = a})

-- | A time you added a public key to CloudFront.
pkCreatedTime :: Lens' PublicKey UTCTime
pkCreatedTime = lens _pkCreatedTime (\ s a -> s{_pkCreatedTime = a}) . _Time

-- | A complex data type for a public key you add to CloudFront to use with features like field-level encryption.
pkPublicKeyConfig :: Lens' PublicKey PublicKeyConfig
pkPublicKeyConfig = lens _pkPublicKeyConfig (\ s a -> s{_pkPublicKeyConfig = a})

instance FromXML PublicKey where
        parseXML x
          = PublicKey' <$>
              (x .@ "Id") <*> (x .@ "CreatedTime") <*>
                (x .@ "PublicKeyConfig")

instance Hashable PublicKey where

instance NFData PublicKey where

-- | Information about a public key you add to CloudFront to use with features like field-level encryption.
--
--
--
-- /See:/ 'publicKeyConfig' smart constructor.
data PublicKeyConfig = PublicKeyConfig'
  { _pkcComment         :: !(Maybe Text)
  , _pkcCallerReference :: !Text
  , _pkcName            :: !Text
  , _pkcEncodedKey      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicKeyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pkcComment' - An optional comment about a public key.
--
-- * 'pkcCallerReference' - A unique number that ensures the request can't be replayed.
--
-- * 'pkcName' - The name for a public key you add to CloudFront to use with features like field-level encryption.
--
-- * 'pkcEncodedKey' - The encoded public key that you want to add to CloudFront to use with features like field-level encryption.
publicKeyConfig
    :: Text -- ^ 'pkcCallerReference'
    -> Text -- ^ 'pkcName'
    -> Text -- ^ 'pkcEncodedKey'
    -> PublicKeyConfig
publicKeyConfig pCallerReference_ pName_ pEncodedKey_ =
  PublicKeyConfig'
    { _pkcComment = Nothing
    , _pkcCallerReference = pCallerReference_
    , _pkcName = pName_
    , _pkcEncodedKey = pEncodedKey_
    }


-- | An optional comment about a public key.
pkcComment :: Lens' PublicKeyConfig (Maybe Text)
pkcComment = lens _pkcComment (\ s a -> s{_pkcComment = a})

-- | A unique number that ensures the request can't be replayed.
pkcCallerReference :: Lens' PublicKeyConfig Text
pkcCallerReference = lens _pkcCallerReference (\ s a -> s{_pkcCallerReference = a})

-- | The name for a public key you add to CloudFront to use with features like field-level encryption.
pkcName :: Lens' PublicKeyConfig Text
pkcName = lens _pkcName (\ s a -> s{_pkcName = a})

-- | The encoded public key that you want to add to CloudFront to use with features like field-level encryption.
pkcEncodedKey :: Lens' PublicKeyConfig Text
pkcEncodedKey = lens _pkcEncodedKey (\ s a -> s{_pkcEncodedKey = a})

instance FromXML PublicKeyConfig where
        parseXML x
          = PublicKeyConfig' <$>
              (x .@? "Comment") <*> (x .@ "CallerReference") <*>
                (x .@ "Name")
                <*> (x .@ "EncodedKey")

instance Hashable PublicKeyConfig where

instance NFData PublicKeyConfig where

instance ToXML PublicKeyConfig where
        toXML PublicKeyConfig'{..}
          = mconcat
              ["Comment" @= _pkcComment,
               "CallerReference" @= _pkcCallerReference,
               "Name" @= _pkcName, "EncodedKey" @= _pkcEncodedKey]

-- | A list of public keys you've added to CloudFront to use with features like field-level encryption.
--
--
--
-- /See:/ 'publicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { _pklItems      :: !(Maybe [PublicKeySummary])
  , _pklNextMarker :: !(Maybe Text)
  , _pklMaxItems   :: !Int
  , _pklQuantity   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicKeyList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pklItems' - An array of information about a public key you add to CloudFront to use with features like field-level encryption.
--
-- * 'pklNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
--
-- * 'pklMaxItems' - The maximum number of public keys you want in the response body.
--
-- * 'pklQuantity' - The number of public keys you added to CloudFront to use with features like field-level encryption.
publicKeyList
    :: Int -- ^ 'pklMaxItems'
    -> Int -- ^ 'pklQuantity'
    -> PublicKeyList
publicKeyList pMaxItems_ pQuantity_ =
  PublicKeyList'
    { _pklItems = Nothing
    , _pklNextMarker = Nothing
    , _pklMaxItems = pMaxItems_
    , _pklQuantity = pQuantity_
    }


-- | An array of information about a public key you add to CloudFront to use with features like field-level encryption.
pklItems :: Lens' PublicKeyList [PublicKeySummary]
pklItems = lens _pklItems (\ s a -> s{_pklItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
pklNextMarker :: Lens' PublicKeyList (Maybe Text)
pklNextMarker = lens _pklNextMarker (\ s a -> s{_pklNextMarker = a})

-- | The maximum number of public keys you want in the response body.
pklMaxItems :: Lens' PublicKeyList Int
pklMaxItems = lens _pklMaxItems (\ s a -> s{_pklMaxItems = a})

-- | The number of public keys you added to CloudFront to use with features like field-level encryption.
pklQuantity :: Lens' PublicKeyList Int
pklQuantity = lens _pklQuantity (\ s a -> s{_pklQuantity = a})

instance FromXML PublicKeyList where
        parseXML x
          = PublicKeyList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "PublicKeySummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "Quantity")

instance Hashable PublicKeyList where

instance NFData PublicKeyList where

-- | Public key information summary.
--
--
--
-- /See:/ 'publicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { _pksComment     :: !(Maybe Text)
  , _pksId          :: !Text
  , _pksName        :: !Text
  , _pksCreatedTime :: !ISO8601
  , _pksEncodedKey  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublicKeySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pksComment' - Comment for public key information summary.
--
-- * 'pksId' - ID for public key information summary.
--
-- * 'pksName' - Name for public key information summary.
--
-- * 'pksCreatedTime' - Creation time for public key information summary.
--
-- * 'pksEncodedKey' - Encoded key for public key information summary.
publicKeySummary
    :: Text -- ^ 'pksId'
    -> Text -- ^ 'pksName'
    -> UTCTime -- ^ 'pksCreatedTime'
    -> Text -- ^ 'pksEncodedKey'
    -> PublicKeySummary
publicKeySummary pId_ pName_ pCreatedTime_ pEncodedKey_ =
  PublicKeySummary'
    { _pksComment = Nothing
    , _pksId = pId_
    , _pksName = pName_
    , _pksCreatedTime = _Time # pCreatedTime_
    , _pksEncodedKey = pEncodedKey_
    }


-- | Comment for public key information summary.
pksComment :: Lens' PublicKeySummary (Maybe Text)
pksComment = lens _pksComment (\ s a -> s{_pksComment = a})

-- | ID for public key information summary.
pksId :: Lens' PublicKeySummary Text
pksId = lens _pksId (\ s a -> s{_pksId = a})

-- | Name for public key information summary.
pksName :: Lens' PublicKeySummary Text
pksName = lens _pksName (\ s a -> s{_pksName = a})

-- | Creation time for public key information summary.
pksCreatedTime :: Lens' PublicKeySummary UTCTime
pksCreatedTime = lens _pksCreatedTime (\ s a -> s{_pksCreatedTime = a}) . _Time

-- | Encoded key for public key information summary.
pksEncodedKey :: Lens' PublicKeySummary Text
pksEncodedKey = lens _pksEncodedKey (\ s a -> s{_pksEncodedKey = a})

instance FromXML PublicKeySummary where
        parseXML x
          = PublicKeySummary' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*> (x .@ "Name")
                <*> (x .@ "CreatedTime")
                <*> (x .@ "EncodedKey")

instance Hashable PublicKeySummary where

instance NFData PublicKeySummary where

-- | Query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfile' smart constructor.
data QueryArgProfile = QueryArgProfile'
  { _qapQueryArg  :: !Text
  , _qapProfileId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryArgProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapQueryArg' - Query argument for field-level encryption query argument-profile mapping.
--
-- * 'qapProfileId' - ID of profile to use for field-level encryption query argument-profile mapping
queryArgProfile
    :: Text -- ^ 'qapQueryArg'
    -> Text -- ^ 'qapProfileId'
    -> QueryArgProfile
queryArgProfile pQueryArg_ pProfileId_ =
  QueryArgProfile' {_qapQueryArg = pQueryArg_, _qapProfileId = pProfileId_}


-- | Query argument for field-level encryption query argument-profile mapping.
qapQueryArg :: Lens' QueryArgProfile Text
qapQueryArg = lens _qapQueryArg (\ s a -> s{_qapQueryArg = a})

-- | ID of profile to use for field-level encryption query argument-profile mapping
qapProfileId :: Lens' QueryArgProfile Text
qapProfileId = lens _qapProfileId (\ s a -> s{_qapProfileId = a})

instance FromXML QueryArgProfile where
        parseXML x
          = QueryArgProfile' <$>
              (x .@ "QueryArg") <*> (x .@ "ProfileId")

instance Hashable QueryArgProfile where

instance NFData QueryArgProfile where

instance ToXML QueryArgProfile where
        toXML QueryArgProfile'{..}
          = mconcat
              ["QueryArg" @= _qapQueryArg,
               "ProfileId" @= _qapProfileId]

-- | Configuration for query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfileConfig' smart constructor.
data QueryArgProfileConfig = QueryArgProfileConfig'
  { _qapcQueryArgProfiles                    :: !(Maybe QueryArgProfiles)
  , _qapcForwardWhenQueryArgProfileIsUnknown :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryArgProfileConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapcQueryArgProfiles' - Profiles specified for query argument-profile mapping for field-level encryption.
--
-- * 'qapcForwardWhenQueryArgProfileIsUnknown' - Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
queryArgProfileConfig
    :: Bool -- ^ 'qapcForwardWhenQueryArgProfileIsUnknown'
    -> QueryArgProfileConfig
queryArgProfileConfig pForwardWhenQueryArgProfileIsUnknown_ =
  QueryArgProfileConfig'
    { _qapcQueryArgProfiles = Nothing
    , _qapcForwardWhenQueryArgProfileIsUnknown =
        pForwardWhenQueryArgProfileIsUnknown_
    }


-- | Profiles specified for query argument-profile mapping for field-level encryption.
qapcQueryArgProfiles :: Lens' QueryArgProfileConfig (Maybe QueryArgProfiles)
qapcQueryArgProfiles = lens _qapcQueryArgProfiles (\ s a -> s{_qapcQueryArgProfiles = a})

-- | Flag to set if you want a request to be forwarded to the origin even if the profile specified by the field-level encryption query argument, fle-profile, is unknown.
qapcForwardWhenQueryArgProfileIsUnknown :: Lens' QueryArgProfileConfig Bool
qapcForwardWhenQueryArgProfileIsUnknown = lens _qapcForwardWhenQueryArgProfileIsUnknown (\ s a -> s{_qapcForwardWhenQueryArgProfileIsUnknown = a})

instance FromXML QueryArgProfileConfig where
        parseXML x
          = QueryArgProfileConfig' <$>
              (x .@? "QueryArgProfiles") <*>
                (x .@ "ForwardWhenQueryArgProfileIsUnknown")

instance Hashable QueryArgProfileConfig where

instance NFData QueryArgProfileConfig where

instance ToXML QueryArgProfileConfig where
        toXML QueryArgProfileConfig'{..}
          = mconcat
              ["QueryArgProfiles" @= _qapcQueryArgProfiles,
               "ForwardWhenQueryArgProfileIsUnknown" @=
                 _qapcForwardWhenQueryArgProfileIsUnknown]

-- | Query argument-profile mapping for field-level encryption.
--
--
--
-- /See:/ 'queryArgProfiles' smart constructor.
data QueryArgProfiles = QueryArgProfiles'
  { _qapItems    :: !(Maybe [QueryArgProfile])
  , _qapQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryArgProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qapItems' - Number of items for query argument-profile mapping for field-level encryption.
--
-- * 'qapQuantity' - Number of profiles for query argument-profile mapping for field-level encryption.
queryArgProfiles
    :: Int -- ^ 'qapQuantity'
    -> QueryArgProfiles
queryArgProfiles pQuantity_ =
  QueryArgProfiles' {_qapItems = Nothing, _qapQuantity = pQuantity_}


-- | Number of items for query argument-profile mapping for field-level encryption.
qapItems :: Lens' QueryArgProfiles [QueryArgProfile]
qapItems = lens _qapItems (\ s a -> s{_qapItems = a}) . _Default . _Coerce

-- | Number of profiles for query argument-profile mapping for field-level encryption.
qapQuantity :: Lens' QueryArgProfiles Int
qapQuantity = lens _qapQuantity (\ s a -> s{_qapQuantity = a})

instance FromXML QueryArgProfiles where
        parseXML x
          = QueryArgProfiles' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "QueryArgProfile"))
                <*> (x .@ "Quantity")

instance Hashable QueryArgProfiles where

instance NFData QueryArgProfiles where

instance ToXML QueryArgProfiles where
        toXML QueryArgProfiles'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "QueryArgProfile" <$> _qapItems),
               "Quantity" @= _qapQuantity]

-- | /See:/ 'queryStringCacheKeys' smart constructor.
data QueryStringCacheKeys = QueryStringCacheKeys'
  { _qsckItems    :: !(Maybe [Text])
  , _qsckQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryStringCacheKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsckItems' - (Optional) A list that contains the query string parameters that you want CloudFront to use as a basis for caching for this cache behavior. If @Quantity@ is 0, you can omit @Items@ .
--
-- * 'qsckQuantity' - The number of @whitelisted@ query string parameters for this cache behavior.
queryStringCacheKeys
    :: Int -- ^ 'qsckQuantity'
    -> QueryStringCacheKeys
queryStringCacheKeys pQuantity_ =
  QueryStringCacheKeys' {_qsckItems = Nothing, _qsckQuantity = pQuantity_}


-- | (Optional) A list that contains the query string parameters that you want CloudFront to use as a basis for caching for this cache behavior. If @Quantity@ is 0, you can omit @Items@ .
qsckItems :: Lens' QueryStringCacheKeys [Text]
qsckItems = lens _qsckItems (\ s a -> s{_qsckItems = a}) . _Default . _Coerce

-- | The number of @whitelisted@ query string parameters for this cache behavior.
qsckQuantity :: Lens' QueryStringCacheKeys Int
qsckQuantity = lens _qsckQuantity (\ s a -> s{_qsckQuantity = a})

instance FromXML QueryStringCacheKeys where
        parseXML x
          = QueryStringCacheKeys' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance Hashable QueryStringCacheKeys where

instance NFData QueryStringCacheKeys where

instance ToXML QueryStringCacheKeys where
        toXML QueryStringCacheKeys'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _qsckItems),
               "Quantity" @= _qsckQuantity]

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
--
--
-- /See:/ 'restrictions' smart constructor.
newtype Restrictions = Restrictions'
  { _rGeoRestriction :: GeoRestriction
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Restrictions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rGeoRestriction' - Undocumented member.
restrictions
    :: GeoRestriction -- ^ 'rGeoRestriction'
    -> Restrictions
restrictions pGeoRestriction_ =
  Restrictions' {_rGeoRestriction = pGeoRestriction_}


-- | Undocumented member.
rGeoRestriction :: Lens' Restrictions GeoRestriction
rGeoRestriction = lens _rGeoRestriction (\ s a -> s{_rGeoRestriction = a})

instance FromXML Restrictions where
        parseXML x
          = Restrictions' <$> (x .@ "GeoRestriction")

instance Hashable Restrictions where

instance NFData Restrictions where

instance ToXML Restrictions where
        toXML Restrictions'{..}
          = mconcat ["GeoRestriction" @= _rGeoRestriction]

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
--
--
-- /See:/ 's3Origin' smart constructor.
data S3Origin = S3Origin'
  { _soDomainName           :: !Text
  , _soOriginAccessIdentity :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Origin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'soDomainName' - The DNS name of the Amazon S3 origin.
--
-- * 'soOriginAccessIdentity' - The CloudFront origin access identity to associate with the RTMP distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront. If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon Amazon CloudFront Developer Guide/ .
s3Origin
    :: Text -- ^ 'soDomainName'
    -> Text -- ^ 'soOriginAccessIdentity'
    -> S3Origin
s3Origin pDomainName_ pOriginAccessIdentity_ =
  S3Origin'
    { _soDomainName = pDomainName_
    , _soOriginAccessIdentity = pOriginAccessIdentity_
    }


-- | The DNS name of the Amazon S3 origin.
soDomainName :: Lens' S3Origin Text
soDomainName = lens _soDomainName (\ s a -> s{_soDomainName = a})

-- | The CloudFront origin access identity to associate with the RTMP distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront. If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon Amazon CloudFront Developer Guide/ .
soOriginAccessIdentity :: Lens' S3Origin Text
soOriginAccessIdentity = lens _soOriginAccessIdentity (\ s a -> s{_soOriginAccessIdentity = a})

instance FromXML S3Origin where
        parseXML x
          = S3Origin' <$>
              (x .@ "DomainName") <*> (x .@ "OriginAccessIdentity")

instance Hashable S3Origin where

instance NFData S3Origin where

instance ToXML S3Origin where
        toXML S3Origin'{..}
          = mconcat
              ["DomainName" @= _soDomainName,
               "OriginAccessIdentity" @= _soOriginAccessIdentity]

-- | A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin, use the @CustomOriginConfig@ element instead.
--
--
--
-- /See:/ 's3OriginConfig' smart constructor.
newtype S3OriginConfig = S3OriginConfig'
  { _socOriginAccessIdentity :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3OriginConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'socOriginAccessIdentity' - The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is: origin-access-identity/cloudfront//ID-of-origin-access-identity/  where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity. If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information about the origin access identity, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
s3OriginConfig
    :: Text -- ^ 'socOriginAccessIdentity'
    -> S3OriginConfig
s3OriginConfig pOriginAccessIdentity_ =
  S3OriginConfig' {_socOriginAccessIdentity = pOriginAccessIdentity_}


-- | The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is: origin-access-identity/cloudfront//ID-of-origin-access-identity/  where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity. If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element. To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element. To replace the origin access identity, update the distribution configuration and specify the new origin access identity. For more information about the origin access identity, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
socOriginAccessIdentity :: Lens' S3OriginConfig Text
socOriginAccessIdentity = lens _socOriginAccessIdentity (\ s a -> s{_socOriginAccessIdentity = a})

instance FromXML S3OriginConfig where
        parseXML x
          = S3OriginConfig' <$> (x .@ "OriginAccessIdentity")

instance Hashable S3OriginConfig where

instance NFData S3OriginConfig where

instance ToXML S3OriginConfig where
        toXML S3OriginConfig'{..}
          = mconcat
              ["OriginAccessIdentity" @= _socOriginAccessIdentity]

-- | A complex type that lists the AWS accounts that were included in the @TrustedSigners@ complex type, as well as their active CloudFront key pair IDs, if any.
--
--
--
-- /See:/ 'signer' smart constructor.
data Signer = Signer'
  { _sAWSAccountNumber :: !(Maybe Text)
  , _sKeyPairIds       :: !(Maybe KeyPairIds)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Signer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAWSAccountNumber' - An AWS account that is included in the @TrustedSigners@ complex type for this RTMP distribution. Valid values include:     * @self@ , which is the AWS account used to create the distribution.     * An AWS account number.
--
-- * 'sKeyPairIds' - A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ .
signer
    :: Signer
signer = Signer' {_sAWSAccountNumber = Nothing, _sKeyPairIds = Nothing}


-- | An AWS account that is included in the @TrustedSigners@ complex type for this RTMP distribution. Valid values include:     * @self@ , which is the AWS account used to create the distribution.     * An AWS account number.
sAWSAccountNumber :: Lens' Signer (Maybe Text)
sAWSAccountNumber = lens _sAWSAccountNumber (\ s a -> s{_sAWSAccountNumber = a})

-- | A complex type that lists the active CloudFront key pairs, if any, that are associated with @AwsAccountNumber@ .
sKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sKeyPairIds = lens _sKeyPairIds (\ s a -> s{_sKeyPairIds = a})

instance FromXML Signer where
        parseXML x
          = Signer' <$>
              (x .@? "AwsAccountNumber") <*> (x .@? "KeyPairIds")

instance Hashable Signer where

instance NFData Signer where

-- | A streaming distribution.
--
--
--
-- /See:/ 'streamingDistribution' smart constructor.
data StreamingDistribution = StreamingDistribution'
  { _sdLastModifiedTime            :: !(Maybe ISO8601)
  , _sdId                          :: !Text
  , _sdARN                         :: !Text
  , _sdStatus                      :: !Text
  , _sdDomainName                  :: !Text
  , _sdActiveTrustedSigners        :: !ActiveTrustedSigners
  , _sdStreamingDistributionConfig :: !StreamingDistributionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLastModifiedTime' - The date and time that the distribution was last modified.
--
-- * 'sdId' - The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
--
-- * 'sdARN' - Undocumented member.
--
-- * 'sdStatus' - The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
--
-- * 'sdDomainName' - The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
--
-- * 'sdActiveTrustedSigners' - A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content. The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'sdStreamingDistributionConfig' - The current configuration information for the RTMP distribution.
streamingDistribution
    :: Text -- ^ 'sdId'
    -> Text -- ^ 'sdARN'
    -> Text -- ^ 'sdStatus'
    -> Text -- ^ 'sdDomainName'
    -> ActiveTrustedSigners -- ^ 'sdActiveTrustedSigners'
    -> StreamingDistributionConfig -- ^ 'sdStreamingDistributionConfig'
    -> StreamingDistribution
streamingDistribution pId_ pARN_ pStatus_ pDomainName_ pActiveTrustedSigners_ pStreamingDistributionConfig_ =
  StreamingDistribution'
    { _sdLastModifiedTime = Nothing
    , _sdId = pId_
    , _sdARN = pARN_
    , _sdStatus = pStatus_
    , _sdDomainName = pDomainName_
    , _sdActiveTrustedSigners = pActiveTrustedSigners_
    , _sdStreamingDistributionConfig = pStreamingDistributionConfig_
    }


-- | The date and time that the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe UTCTime)
sdLastModifiedTime = lens _sdLastModifiedTime (\ s a -> s{_sdLastModifiedTime = a}) . mapping _Time

-- | The identifier for the RTMP distribution. For example: @EGTXBD79EXAMPLE@ .
sdId :: Lens' StreamingDistribution Text
sdId = lens _sdId (\ s a -> s{_sdId = a})

-- | Undocumented member.
sdARN :: Lens' StreamingDistribution Text
sdARN = lens _sdARN (\ s a -> s{_sdARN = a})

-- | The current status of the RTMP distribution. When the status is @Deployed@ , the distribution's information is propagated to all CloudFront edge locations.
sdStatus :: Lens' StreamingDistribution Text
sdStatus = lens _sdStatus (\ s a -> s{_sdStatus = a})

-- | The domain name that corresponds to the streaming distribution, for example, @s5c39gqb8ow64r.cloudfront.net@ .
sdDomainName :: Lens' StreamingDistribution Text
sdDomainName = lens _sdDomainName (\ s a -> s{_sdDomainName = a})

-- | A complex type that lists the AWS accounts, if any, that you included in the @TrustedSigners@ complex type for this distribution. These are the accounts that you want to allow to create signed URLs for private content. The @Signer@ complex type lists the AWS account number of the trusted signer or @self@ if the signer is the AWS account that created the distribution. The @Signer@ element also includes the IDs of any active CloudFront key pairs that are associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create signed URLs. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
sdActiveTrustedSigners :: Lens' StreamingDistribution ActiveTrustedSigners
sdActiveTrustedSigners = lens _sdActiveTrustedSigners (\ s a -> s{_sdActiveTrustedSigners = a})

-- | The current configuration information for the RTMP distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution StreamingDistributionConfig
sdStreamingDistributionConfig = lens _sdStreamingDistributionConfig (\ s a -> s{_sdStreamingDistributionConfig = a})

instance FromXML StreamingDistribution where
        parseXML x
          = StreamingDistribution' <$>
              (x .@? "LastModifiedTime") <*> (x .@ "Id") <*>
                (x .@ "ARN")
                <*> (x .@ "Status")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "StreamingDistributionConfig")

instance Hashable StreamingDistribution where

instance NFData StreamingDistribution where

-- | The RTMP distribution's configuration information.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingDistributionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- * 'sdcPriceClass' - A complex type that contains information about price class for this streaming distribution.
--
-- * 'sdcLogging' - A complex type that controls whether access logs are written for the streaming distribution.
--
-- * 'sdcCallerReference' - A unique number that ensures that the request can't be replayed. If the @CallerReference@ is new (no matter the content of the @StreamingDistributionConfig@ object), a new streaming distribution is created. If the @CallerReference@ is a value that you already sent in a previous request to create a streaming distribution, and the content of the @StreamingDistributionConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request. If the @CallerReference@ is a value that you already sent in a previous request to create a streaming distribution but the content of the @StreamingDistributionConfig@ is different from the original request, CloudFront returns a @DistributionAlreadyExists@ error.
--
-- * 'sdcS3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- * 'sdcComment' - Any comments you want to include about the streaming distribution.
--
-- * 'sdcTrustedSigners' - A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'sdcEnabled' - Whether the streaming distribution is enabled to accept user requests for content.
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


-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Maybe Aliases)
sdcAliases = lens _sdcAliases (\ s a -> s{_sdcAliases = a})

-- | A complex type that contains information about price class for this streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (Maybe PriceClass)
sdcPriceClass = lens _sdcPriceClass (\ s a -> s{_sdcPriceClass = a})

-- | A complex type that controls whether access logs are written for the streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (Maybe StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\ s a -> s{_sdcLogging = a})

-- | A unique number that ensures that the request can't be replayed. If the @CallerReference@ is new (no matter the content of the @StreamingDistributionConfig@ object), a new streaming distribution is created. If the @CallerReference@ is a value that you already sent in a previous request to create a streaming distribution, and the content of the @StreamingDistributionConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request. If the @CallerReference@ is a value that you already sent in a previous request to create a streaming distribution but the content of the @StreamingDistributionConfig@ is different from the original request, CloudFront returns a @DistributionAlreadyExists@ error.
sdcCallerReference :: Lens' StreamingDistributionConfig Text
sdcCallerReference = lens _sdcCallerReference (\ s a -> s{_sdcCallerReference = a})

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = lens _sdcS3Origin (\ s a -> s{_sdcS3Origin = a})

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig Text
sdcComment = lens _sdcComment (\ s a -> s{_sdcComment = a})

-- | A complex type that specifies any AWS accounts that you want to permit to create signed URLs for private content. If you want the distribution to use signed URLs, include this element; if you want the distribution to use public URLs, remove this element. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
sdcTrustedSigners :: Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners = lens _sdcTrustedSigners (\ s a -> s{_sdcTrustedSigners = a})

-- | Whether the streaming distribution is enabled to accept user requests for content.
sdcEnabled :: Lens' StreamingDistributionConfig Bool
sdcEnabled = lens _sdcEnabled (\ s a -> s{_sdcEnabled = a})

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

instance Hashable StreamingDistributionConfig where

instance NFData StreamingDistributionConfig where

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

-- | A streaming distribution Configuration and a list of tags to be associated with the streaming distribution.
--
--
--
-- /See:/ 'streamingDistributionConfigWithTags' smart constructor.
data StreamingDistributionConfigWithTags = StreamingDistributionConfigWithTags'
  { _sdcwtStreamingDistributionConfig :: !StreamingDistributionConfig
  , _sdcwtTags                        :: !Tags
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingDistributionConfigWithTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcwtStreamingDistributionConfig' - A streaming distribution Configuration.
--
-- * 'sdcwtTags' - A complex type that contains zero or more @Tag@ elements.
streamingDistributionConfigWithTags
    :: StreamingDistributionConfig -- ^ 'sdcwtStreamingDistributionConfig'
    -> Tags -- ^ 'sdcwtTags'
    -> StreamingDistributionConfigWithTags
streamingDistributionConfigWithTags pStreamingDistributionConfig_ pTags_ =
  StreamingDistributionConfigWithTags'
    { _sdcwtStreamingDistributionConfig = pStreamingDistributionConfig_
    , _sdcwtTags = pTags_
    }


-- | A streaming distribution Configuration.
sdcwtStreamingDistributionConfig :: Lens' StreamingDistributionConfigWithTags StreamingDistributionConfig
sdcwtStreamingDistributionConfig = lens _sdcwtStreamingDistributionConfig (\ s a -> s{_sdcwtStreamingDistributionConfig = a})

-- | A complex type that contains zero or more @Tag@ elements.
sdcwtTags :: Lens' StreamingDistributionConfigWithTags Tags
sdcwtTags = lens _sdcwtTags (\ s a -> s{_sdcwtTags = a})

instance Hashable StreamingDistributionConfigWithTags
         where

instance NFData StreamingDistributionConfigWithTags
         where

instance ToXML StreamingDistributionConfigWithTags
         where
        toXML StreamingDistributionConfigWithTags'{..}
          = mconcat
              ["StreamingDistributionConfig" @=
                 _sdcwtStreamingDistributionConfig,
               "Tags" @= _sdcwtTags]

-- | A streaming distribution list.
--
--
--
-- /See:/ 'streamingDistributionList' smart constructor.
data StreamingDistributionList = StreamingDistributionList'
  { _sdlItems       :: !(Maybe [StreamingDistributionSummary])
  , _sdlNextMarker  :: !(Maybe Text)
  , _sdlMarker      :: !Text
  , _sdlMaxItems    :: !Int
  , _sdlIsTruncated :: !Bool
  , _sdlQuantity    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingDistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdlItems' - A complex type that contains one @StreamingDistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- * 'sdlNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your RTMP distributions where they left off.
--
-- * 'sdlMarker' - The value you provided for the @Marker@ request parameter.
--
-- * 'sdlMaxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- * 'sdlIsTruncated' - A flag that indicates whether more streaming distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- * 'sdlQuantity' - The number of streaming distributions that were created by the current AWS account.
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


-- | A complex type that contains one @StreamingDistributionSummary@ element for each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList [StreamingDistributionSummary]
sdlItems = lens _sdlItems (\ s a -> s{_sdlItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your RTMP distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\ s a -> s{_sdlNextMarker = a})

-- | The value you provided for the @Marker@ request parameter.
sdlMarker :: Lens' StreamingDistributionList Text
sdlMarker = lens _sdlMarker (\ s a -> s{_sdlMarker = a})

-- | The value you provided for the @MaxItems@ request parameter.
sdlMaxItems :: Lens' StreamingDistributionList Int
sdlMaxItems = lens _sdlMaxItems (\ s a -> s{_sdlMaxItems = a})

-- | A flag that indicates whether more streaming distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
sdlIsTruncated :: Lens' StreamingDistributionList Bool
sdlIsTruncated = lens _sdlIsTruncated (\ s a -> s{_sdlIsTruncated = a})

-- | The number of streaming distributions that were created by the current AWS account.
sdlQuantity :: Lens' StreamingDistributionList Int
sdlQuantity = lens _sdlQuantity (\ s a -> s{_sdlQuantity = a})

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

instance Hashable StreamingDistributionList where

instance NFData StreamingDistributionList where

-- | A summary of the information for an Amazon CloudFront streaming distribution.
--
--
--
-- /See:/ 'streamingDistributionSummary' smart constructor.
data StreamingDistributionSummary = StreamingDistributionSummary'
  { _sdsId               :: !Text
  , _sdsARN              :: !Text
  , _sdsStatus           :: !Text
  , _sdsLastModifiedTime :: !ISO8601
  , _sdsDomainName       :: !Text
  , _sdsS3Origin         :: !S3Origin
  , _sdsAliases          :: !Aliases
  , _sdsTrustedSigners   :: !TrustedSigners
  , _sdsComment          :: !Text
  , _sdsPriceClass       :: !PriceClass
  , _sdsEnabled          :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingDistributionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsId' - The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
--
-- * 'sdsARN' - The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'sdsStatus' - Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
--
-- * 'sdsLastModifiedTime' - The date and time the distribution was last modified.
--
-- * 'sdsDomainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- * 'sdsS3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- * 'sdsAliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- * 'sdsTrustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- * 'sdsComment' - The comment originally specified when this distribution was created.
--
-- * 'sdsPriceClass' - Undocumented member.
--
-- * 'sdsEnabled' - Whether the distribution is enabled to accept end user requests for content.
streamingDistributionSummary
    :: Text -- ^ 'sdsId'
    -> Text -- ^ 'sdsARN'
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
streamingDistributionSummary pId_ pARN_ pStatus_ pLastModifiedTime_ pDomainName_ pS3Origin_ pAliases_ pTrustedSigners_ pComment_ pPriceClass_ pEnabled_ =
  StreamingDistributionSummary'
    { _sdsId = pId_
    , _sdsARN = pARN_
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


-- | The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
sdsId :: Lens' StreamingDistributionSummary Text
sdsId = lens _sdsId (\ s a -> s{_sdsId = a})

-- | The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
sdsARN :: Lens' StreamingDistributionSummary Text
sdsARN = lens _sdsARN (\ s a -> s{_sdsARN = a})

-- | Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary Text
sdsStatus = lens _sdsStatus (\ s a -> s{_sdsStatus = a})

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary UTCTime
sdsLastModifiedTime = lens _sdsLastModifiedTime (\ s a -> s{_sdsLastModifiedTime = a}) . _Time

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
sdsDomainName :: Lens' StreamingDistributionSummary Text
sdsDomainName = lens _sdsDomainName (\ s a -> s{_sdsDomainName = a})

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = lens _sdsS3Origin (\ s a -> s{_sdsS3Origin = a})

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary Aliases
sdsAliases = lens _sdsAliases (\ s a -> s{_sdsAliases = a})

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
sdsTrustedSigners :: Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners = lens _sdsTrustedSigners (\ s a -> s{_sdsTrustedSigners = a})

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary Text
sdsComment = lens _sdsComment (\ s a -> s{_sdsComment = a})

-- | Undocumented member.
sdsPriceClass :: Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = lens _sdsPriceClass (\ s a -> s{_sdsPriceClass = a})

-- | Whether the distribution is enabled to accept end user requests for content.
sdsEnabled :: Lens' StreamingDistributionSummary Bool
sdsEnabled = lens _sdsEnabled (\ s a -> s{_sdsEnabled = a})

instance FromXML StreamingDistributionSummary where
        parseXML x
          = StreamingDistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "ARN") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "DomainName")
                <*> (x .@ "S3Origin")
                <*> (x .@ "Aliases")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "Comment")
                <*> (x .@ "PriceClass")
                <*> (x .@ "Enabled")

instance Hashable StreamingDistributionSummary where

instance NFData StreamingDistributionSummary where

-- | A complex type that controls whether access logs are written for this streaming distribution.
--
--
--
-- /See:/ 'streamingLoggingConfig' smart constructor.
data StreamingLoggingConfig = StreamingLoggingConfig'
  { _slcEnabled :: !Bool
  , _slcBucket  :: !Text
  , _slcPrefix  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamingLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcEnabled' - Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
--
-- * 'slcBucket' - The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
--
-- * 'slcPrefix' - An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
streamingLoggingConfig
    :: Bool -- ^ 'slcEnabled'
    -> Text -- ^ 'slcBucket'
    -> Text -- ^ 'slcPrefix'
    -> StreamingLoggingConfig
streamingLoggingConfig pEnabled_ pBucket_ pPrefix_ =
  StreamingLoggingConfig'
    {_slcEnabled = pEnabled_, _slcBucket = pBucket_, _slcPrefix = pPrefix_}


-- | Specifies whether you want CloudFront to save access logs to an Amazon S3 bucket. If you don't want to enable logging when you create a streaming distribution or if you want to disable logging for an existing streaming distribution, specify @false@ for @Enabled@ , and specify @empty Bucket@ and @Prefix@ elements. If you specify @false@ for @Enabled@ but you specify values for @Bucket@ and @Prefix@ , the values are automatically deleted.
slcEnabled :: Lens' StreamingLoggingConfig Bool
slcEnabled = lens _slcEnabled (\ s a -> s{_slcEnabled = a})

-- | The Amazon S3 bucket to store the access logs in, for example, @myawslogbucket.s3.amazonaws.com@ .
slcBucket :: Lens' StreamingLoggingConfig Text
slcBucket = lens _slcBucket (\ s a -> s{_slcBucket = a})

-- | An optional string that you want CloudFront to prefix to the access log filenames for this streaming distribution, for example, @myprefix/@ . If you want to enable logging, but you don't want to specify a prefix, you still must include an empty @Prefix@ element in the @Logging@ element.
slcPrefix :: Lens' StreamingLoggingConfig Text
slcPrefix = lens _slcPrefix (\ s a -> s{_slcPrefix = a})

instance FromXML StreamingLoggingConfig where
        parseXML x
          = StreamingLoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "Bucket") <*>
                (x .@ "Prefix")

instance Hashable StreamingLoggingConfig where

instance NFData StreamingLoggingConfig where

instance ToXML StreamingLoggingConfig where
        toXML StreamingLoggingConfig'{..}
          = mconcat
              ["Enabled" @= _slcEnabled, "Bucket" @= _slcBucket,
               "Prefix" @= _slcPrefix]

-- | A complex type that contains @Tag@ key and @Tag@ value.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A string that contains an optional @Tag@ value. The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
--
-- * 'tagKey' - A string that contains @Tag@ key. The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | A string that contains an optional @Tag@ value. The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A string that contains @Tag@ key. The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@ "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

-- | A complex type that contains zero or more @Tag@ elements.
--
--
--
-- /See:/ 'tagKeys' smart constructor.
newtype TagKeys = TagKeys'
  { _tkItems :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tkItems' - A complex type that contains @Tag@ key elements.
tagKeys
    :: TagKeys
tagKeys = TagKeys' {_tkItems = Nothing}


-- | A complex type that contains @Tag@ key elements.
tkItems :: Lens' TagKeys [Text]
tkItems = lens _tkItems (\ s a -> s{_tkItems = a}) . _Default . _Coerce

instance Hashable TagKeys where

instance NFData TagKeys where

instance ToXML TagKeys where
        toXML TagKeys'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Key" <$> _tkItems)]

-- | A complex type that contains zero or more @Tag@ elements.
--
--
--
-- /See:/ 'tags' smart constructor.
newtype Tags = Tags'
  { _tItems :: Maybe [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tItems' - A complex type that contains @Tag@ elements.
tags
    :: Tags
tags = Tags' {_tItems = Nothing}


-- | A complex type that contains @Tag@ elements.
tItems :: Lens' Tags [Tag]
tItems = lens _tItems (\ s a -> s{_tItems = a}) . _Default . _Coerce

instance FromXML Tags where
        parseXML x
          = Tags' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Tag"))

instance Hashable Tags where

instance NFData Tags where

instance ToXML Tags where
        toXML Tags'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Tag" <$> _tItems)]

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content.
--
--
-- If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon Amazon CloudFront Developer Guide/ .
--
-- If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ .
--
-- To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- For more information about updating the distribution configuration, see 'DistributionConfig' .
--
--
-- /See:/ 'trustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { _tsItems    :: !(Maybe [Text])
  , _tsEnabled  :: !Bool
  , _tsQuantity :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrustedSigners' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsItems' - __Optional__ : A complex type that contains trusted signers for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'tsEnabled' - Specifies whether you want to require viewers to use signed URLs to access the files specified by @PathPattern@ and @TargetOriginId@ .
--
-- * 'tsQuantity' - The number of trusted signers for this cache behavior.
trustedSigners
    :: Bool -- ^ 'tsEnabled'
    -> Int -- ^ 'tsQuantity'
    -> TrustedSigners
trustedSigners pEnabled_ pQuantity_ =
  TrustedSigners'
    {_tsItems = Nothing, _tsEnabled = pEnabled_, _tsQuantity = pQuantity_}


-- | __Optional__ : A complex type that contains trusted signers for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\ s a -> s{_tsItems = a}) . _Default . _Coerce

-- | Specifies whether you want to require viewers to use signed URLs to access the files specified by @PathPattern@ and @TargetOriginId@ .
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\ s a -> s{_tsEnabled = a})

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\ s a -> s{_tsQuantity = a})

instance FromXML TrustedSigners where
        parseXML x
          = TrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "AwsAccountNumber"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance Hashable TrustedSigners where

instance NFData TrustedSigners where

instance ToXML TrustedSigners where
        toXML TrustedSigners'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "AwsAccountNumber" <$> _tsItems),
               "Enabled" @= _tsEnabled, "Quantity" @= _tsQuantity]

-- | A complex type that specifies the following:
--
--
--     * Whether you want viewers to use HTTP or HTTPS to request your objects.
--
--     * If you want viewers to use HTTPS, whether you're using an alternate domain name such as @example.com@ or the CloudFront domain name for your distribution, such as @d111111abcdef8.cloudfront.net@ .
--
--     * If you're using an alternate domain name, whether AWS Certificate Manager (ACM) provided the certificate, or you purchased a certificate from a third-party certificate authority and imported it into ACM or uploaded it to the IAM certificate store.
--
--
--
-- You must specify only one of the following values:
--
--     * 'ViewerCertificate$ACMCertificateArn'
--
--     * 'ViewerCertificate$IAMCertificateId'
--
--     * 'ViewerCertificate$CloudFrontDefaultCertificate'
--
--
--
-- Don't specify @false@ for @CloudFrontDefaultCertificate@ .
--
-- __If you want viewers to use HTTP instead of HTTPS to request your objects__ : Specify the following value:
--
-- @<CloudFrontDefaultCertificate>true<CloudFrontDefaultCertificate>@
--
-- In addition, specify @allow-all@ for @ViewerProtocolPolicy@ for all of your cache behaviors.
--
-- __If you want viewers to use HTTPS to request your objects__ : Choose the type of certificate that you want to use based on whether you're using an alternate domain name for your objects or the CloudFront domain name:
--
--     * __If you're using an alternate domain name, such as example.com__ : Specify one of the following values, depending on whether ACM provided your certificate or you purchased your certificate from third-party certificate authority:
--
--     * @<ACMCertificateArn>/ARN for ACM SSL\/TLS certificate/ <ACMCertificateArn>@ where @/ARN for ACM SSL\/TLS certificate/ @ is the ARN for the ACM SSL/TLS certificate that you want to use for this distribution.
--
--     * @<IAMCertificateId>/IAM certificate ID/ <IAMCertificateId>@ where @/IAM certificate ID/ @ is the ID that IAM returned when you added the certificate to the IAM certificate store.
--
--
--
-- If you specify @ACMCertificateArn@ or @IAMCertificateId@ , you must also specify a value for @SSLSupportMethod@ .
--
-- If you choose to use an ACM certificate or a certificate in the IAM certificate store, we recommend that you use only an alternate domain name in your object URLs (@https://example.com/logo.jpg@ ). If you use the domain name that is associated with your CloudFront distribution (such as @https://d111111abcdef8.cloudfront.net/logo.jpg@ ) and the viewer supports @SNI@ , then CloudFront behaves normally. However, if the browser does not support SNI, the user's experience depends on the value that you choose for @SSLSupportMethod@ :
--
--     * @vip@ : The viewer displays a warning because there is a mismatch between the CloudFront domain name and the domain name in your SSL/TLS certificate.
--
--     * @sni-only@ : CloudFront drops the connection with the browser without returning the object.
--
--
--
--     * __If you're using the CloudFront domain name for your distribution, such as @d111111abcdef8.cloudfront.net@ __ : Specify the following value:
--
-- @<CloudFrontDefaultCertificate>true<CloudFrontDefaultCertificate> @
--
--
--
-- If you want viewers to use HTTPS, you must also specify one of the following values in your cache behaviors:
--
--     * @<ViewerProtocolPolicy>https-only<ViewerProtocolPolicy>@
--
--     * @<ViewerProtocolPolicy>redirect-to-https<ViewerProtocolPolicy>@
--
--
--
-- You can also optionally require that CloudFront use HTTPS to communicate with your origin by specifying one of the following values for the applicable origins:
--
--     * @<OriginProtocolPolicy>https-only<OriginProtocolPolicy> @
--
--     * @<OriginProtocolPolicy>match-viewer<OriginProtocolPolicy> @
--
--
--
-- For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html#CNAMEsAndHTTPS Using Alternate Domain Names and HTTPS> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'viewerCertificate' smart constructor.
data ViewerCertificate = ViewerCertificate'
  { _vcSSLSupportMethod             :: !(Maybe SSLSupportMethod)
  , _vcACMCertificateARN            :: !(Maybe Text)
  , _vcCertificateSource            :: !(Maybe CertificateSource)
  , _vcMinimumProtocolVersion       :: !(Maybe MinimumProtocolVersion)
  , _vcCertificate                  :: !(Maybe Text)
  , _vcIAMCertificateId             :: !(Maybe Text)
  , _vcCloudFrontDefaultCertificate :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ViewerCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSSLSupportMethod' - If you specify a value for 'ViewerCertificate$ACMCertificateArn' or for 'ViewerCertificate$IAMCertificateId' , you must also specify how you want CloudFront to serve HTTPS requests: using a method that works for all clients or one that works for most clients:     * @vip@ : CloudFront uses dedicated IP addresses for your content and can respond to HTTPS requests from any viewer. However, you will incur additional monthly charges.     * @sni-only@ : CloudFront can respond to HTTPS requests from viewers that support Server Name Indication (SNI). All modern browsers support SNI, but some browsers still in use don't support SNI. If some of your users' browsers don't support SNI, we recommend that you do one of the following:     * Use the @vip@ option (dedicated IP addresses) instead of @sni-only@ .     * Use the CloudFront SSL/TLS certificate instead of a custom certificate. This requires that you use the CloudFront domain name of your distribution in the URLs for your objects, for example, @https://d111111abcdef8.cloudfront.net/logo.png@ .     * If you can control which browser your users use, upgrade the browser to one that supports SNI.     * Use HTTP instead of HTTPS. Don't specify a value for @SSLSupportMethod@ if you specified @<CloudFrontDefaultCertificate>true<CloudFrontDefaultCertificate>@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html#CNAMEsAndHTTPS.html Using Alternate Domain Names and HTTPS> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'vcACMCertificateARN' - For information about how and when to use @ACMCertificateArn@ , see 'ViewerCertificate' .
--
-- * 'vcCertificateSource' - This field has been deprecated. Use one of the following fields instead:     * 'ViewerCertificate$ACMCertificateArn'      * 'ViewerCertificate$IAMCertificateId'      * 'ViewerCertificate$CloudFrontDefaultCertificate'
--
-- * 'vcMinimumProtocolVersion' - Specify the security policy that you want CloudFront to use for HTTPS connections. A security policy determines two settings:     * The minimum SSL/TLS protocol that CloudFront uses to communicate with viewers     * The cipher that CloudFront uses to encrypt the content that it returns to viewers We recommend that you specify @TLSv1.1_2016@ unless your users are using browsers or devices that do not support TLSv1.1 or later. When both of the following are true, you must specify @TLSv1@ or later for the security policy:      * You're using a custom certificate: you specified a value for @ACMCertificateArn@ or for @IAMCertificateId@      * You're using SNI: you specified @sni-only@ for @SSLSupportMethod@  If you specify @true@ for @CloudFrontDefaultCertificate@ , CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you specify for @MinimumProtocolVersion@ . For information about the relationship between the security policy that you choose and the protocols and ciphers that CloudFront uses to communicate with viewers, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported SSL/TLS Protocols and Ciphers for Communication Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'vcCertificate' - This field has been deprecated. Use one of the following fields instead:     * 'ViewerCertificate$ACMCertificateArn'      * 'ViewerCertificate$IAMCertificateId'      * 'ViewerCertificate$CloudFrontDefaultCertificate'
--
-- * 'vcIAMCertificateId' - For information about how and when to use @IAMCertificateId@ , see 'ViewerCertificate' .
--
-- * 'vcCloudFrontDefaultCertificate' - For information about how and when to use @CloudFrontDefaultCertificate@ , see 'ViewerCertificate' .
viewerCertificate
    :: ViewerCertificate
viewerCertificate =
  ViewerCertificate'
    { _vcSSLSupportMethod = Nothing
    , _vcACMCertificateARN = Nothing
    , _vcCertificateSource = Nothing
    , _vcMinimumProtocolVersion = Nothing
    , _vcCertificate = Nothing
    , _vcIAMCertificateId = Nothing
    , _vcCloudFrontDefaultCertificate = Nothing
    }


-- | If you specify a value for 'ViewerCertificate$ACMCertificateArn' or for 'ViewerCertificate$IAMCertificateId' , you must also specify how you want CloudFront to serve HTTPS requests: using a method that works for all clients or one that works for most clients:     * @vip@ : CloudFront uses dedicated IP addresses for your content and can respond to HTTPS requests from any viewer. However, you will incur additional monthly charges.     * @sni-only@ : CloudFront can respond to HTTPS requests from viewers that support Server Name Indication (SNI). All modern browsers support SNI, but some browsers still in use don't support SNI. If some of your users' browsers don't support SNI, we recommend that you do one of the following:     * Use the @vip@ option (dedicated IP addresses) instead of @sni-only@ .     * Use the CloudFront SSL/TLS certificate instead of a custom certificate. This requires that you use the CloudFront domain name of your distribution in the URLs for your objects, for example, @https://d111111abcdef8.cloudfront.net/logo.png@ .     * If you can control which browser your users use, upgrade the browser to one that supports SNI.     * Use HTTP instead of HTTPS. Don't specify a value for @SSLSupportMethod@ if you specified @<CloudFrontDefaultCertificate>true<CloudFrontDefaultCertificate>@ . For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/SecureConnections.html#CNAMEsAndHTTPS.html Using Alternate Domain Names and HTTPS> in the /Amazon CloudFront Developer Guide/ .
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod = lens _vcSSLSupportMethod (\ s a -> s{_vcSSLSupportMethod = a})

-- | For information about how and when to use @ACMCertificateArn@ , see 'ViewerCertificate' .
vcACMCertificateARN :: Lens' ViewerCertificate (Maybe Text)
vcACMCertificateARN = lens _vcACMCertificateARN (\ s a -> s{_vcACMCertificateARN = a})

-- | This field has been deprecated. Use one of the following fields instead:     * 'ViewerCertificate$ACMCertificateArn'      * 'ViewerCertificate$IAMCertificateId'      * 'ViewerCertificate$CloudFrontDefaultCertificate'
vcCertificateSource :: Lens' ViewerCertificate (Maybe CertificateSource)
vcCertificateSource = lens _vcCertificateSource (\ s a -> s{_vcCertificateSource = a})

-- | Specify the security policy that you want CloudFront to use for HTTPS connections. A security policy determines two settings:     * The minimum SSL/TLS protocol that CloudFront uses to communicate with viewers     * The cipher that CloudFront uses to encrypt the content that it returns to viewers We recommend that you specify @TLSv1.1_2016@ unless your users are using browsers or devices that do not support TLSv1.1 or later. When both of the following are true, you must specify @TLSv1@ or later for the security policy:      * You're using a custom certificate: you specified a value for @ACMCertificateArn@ or for @IAMCertificateId@      * You're using SNI: you specified @sni-only@ for @SSLSupportMethod@  If you specify @true@ for @CloudFrontDefaultCertificate@ , CloudFront automatically sets the security policy to @TLSv1@ regardless of the value that you specify for @MinimumProtocolVersion@ . For information about the relationship between the security policy that you choose and the protocols and ciphers that CloudFront uses to communicate with viewers, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/secure-connections-supported-viewer-protocols-ciphers.html#secure-connections-supported-ciphers Supported SSL/TLS Protocols and Ciphers for Communication Between Viewers and CloudFront> in the /Amazon CloudFront Developer Guide/ .
vcMinimumProtocolVersion :: Lens' ViewerCertificate (Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion = lens _vcMinimumProtocolVersion (\ s a -> s{_vcMinimumProtocolVersion = a})

-- | This field has been deprecated. Use one of the following fields instead:     * 'ViewerCertificate$ACMCertificateArn'      * 'ViewerCertificate$IAMCertificateId'      * 'ViewerCertificate$CloudFrontDefaultCertificate'
vcCertificate :: Lens' ViewerCertificate (Maybe Text)
vcCertificate = lens _vcCertificate (\ s a -> s{_vcCertificate = a})

-- | For information about how and when to use @IAMCertificateId@ , see 'ViewerCertificate' .
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId = lens _vcIAMCertificateId (\ s a -> s{_vcIAMCertificateId = a})

-- | For information about how and when to use @CloudFrontDefaultCertificate@ , see 'ViewerCertificate' .
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate = lens _vcCloudFrontDefaultCertificate (\ s a -> s{_vcCloudFrontDefaultCertificate = a})

instance FromXML ViewerCertificate where
        parseXML x
          = ViewerCertificate' <$>
              (x .@? "SSLSupportMethod") <*>
                (x .@? "ACMCertificateArn")
                <*> (x .@? "CertificateSource")
                <*> (x .@? "MinimumProtocolVersion")
                <*> (x .@? "Certificate")
                <*> (x .@? "IAMCertificateId")
                <*> (x .@? "CloudFrontDefaultCertificate")

instance Hashable ViewerCertificate where

instance NFData ViewerCertificate where

instance ToXML ViewerCertificate where
        toXML ViewerCertificate'{..}
          = mconcat
              ["SSLSupportMethod" @= _vcSSLSupportMethod,
               "ACMCertificateArn" @= _vcACMCertificateARN,
               "CertificateSource" @= _vcCertificateSource,
               "MinimumProtocolVersion" @=
                 _vcMinimumProtocolVersion,
               "Certificate" @= _vcCertificate,
               "IAMCertificateId" @= _vcIAMCertificateId,
               "CloudFrontDefaultCertificate" @=
                 _vcCloudFrontDefaultCertificate]
