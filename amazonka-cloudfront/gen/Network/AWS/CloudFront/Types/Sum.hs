{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Sum where

import           Network.AWS.Prelude

data GeoRestrictionType
    = None
    | Whitelist
    | Blacklist
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText GeoRestrictionType where
    parser = takeLowerText >>= \case
        "blacklist" -> pure Blacklist
        "none" -> pure None
        "whitelist" -> pure Whitelist
        e -> fromTextError $ "Failure parsing GeoRestrictionType from value: '" <> e
           <> "'. Accepted values: blacklist, none, whitelist"

instance ToText GeoRestrictionType where
    toText = \case
        Blacklist -> "blacklist"
        None -> "none"
        Whitelist -> "whitelist"

instance Hashable     GeoRestrictionType
instance ToByteString GeoRestrictionType
instance ToQuery      GeoRestrictionType
instance ToHeader     GeoRestrictionType

instance FromXML GeoRestrictionType where
    parseXML = parseXMLText "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXML = toXMLText

data ItemSelection
    = ISWhitelist
    | ISNone
    | ISAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ItemSelection where
    parser = takeLowerText >>= \case
        "all" -> pure ISAll
        "none" -> pure ISNone
        "whitelist" -> pure ISWhitelist
        e -> fromTextError $ "Failure parsing ItemSelection from value: '" <> e
           <> "'. Accepted values: all, none, whitelist"

instance ToText ItemSelection where
    toText = \case
        ISAll -> "all"
        ISNone -> "none"
        ISWhitelist -> "whitelist"

instance Hashable     ItemSelection
instance ToByteString ItemSelection
instance ToQuery      ItemSelection
instance ToHeader     ItemSelection

instance FromXML ItemSelection where
    parseXML = parseXMLText "ItemSelection"

instance ToXML ItemSelection where
    toXML = toXMLText

data Method
    = Head
    | Post
    | Patch
    | Get
    | Options
    | Put
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Method where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "get" -> pure Get
        "head" -> pure Head
        "options" -> pure Options
        "patch" -> pure Patch
        "post" -> pure Post
        "put" -> pure Put
        e -> fromTextError $ "Failure parsing Method from value: '" <> e
           <> "'. Accepted values: delete, get, head, options, patch, post, put"

instance ToText Method where
    toText = \case
        Delete -> "delete"
        Get -> "get"
        Head -> "head"
        Options -> "options"
        Patch -> "patch"
        Post -> "post"
        Put -> "put"

instance Hashable     Method
instance ToByteString Method
instance ToQuery      Method
instance ToHeader     Method

instance FromXML Method where
    parseXML = parseXMLText "Method"

instance ToXML Method where
    toXML = toXMLText

data MinimumProtocolVersion
    = TLSV1
    | SSLV3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MinimumProtocolVersion where
    parser = takeLowerText >>= \case
        "sslv3" -> pure SSLV3
        "tlsv1" -> pure TLSV1
        e -> fromTextError $ "Failure parsing MinimumProtocolVersion from value: '" <> e
           <> "'. Accepted values: sslv3, tlsv1"

instance ToText MinimumProtocolVersion where
    toText = \case
        SSLV3 -> "sslv3"
        TLSV1 -> "tlsv1"

instance Hashable     MinimumProtocolVersion
instance ToByteString MinimumProtocolVersion
instance ToQuery      MinimumProtocolVersion
instance ToHeader     MinimumProtocolVersion

instance FromXML MinimumProtocolVersion where
    parseXML = parseXMLText "MinimumProtocolVersion"

instance ToXML MinimumProtocolVersion where
    toXML = toXMLText

data OriginProtocolPolicy
    = HTTPOnly
    | MatchViewer
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OriginProtocolPolicy where
    parser = takeLowerText >>= \case
        "http-only" -> pure HTTPOnly
        "match-viewer" -> pure MatchViewer
        e -> fromTextError $ "Failure parsing OriginProtocolPolicy from value: '" <> e
           <> "'. Accepted values: http-only, match-viewer"

instance ToText OriginProtocolPolicy where
    toText = \case
        HTTPOnly -> "http-only"
        MatchViewer -> "match-viewer"

instance Hashable     OriginProtocolPolicy
instance ToByteString OriginProtocolPolicy
instance ToQuery      OriginProtocolPolicy
instance ToHeader     OriginProtocolPolicy

instance FromXML OriginProtocolPolicy where
    parseXML = parseXMLText "OriginProtocolPolicy"

instance ToXML OriginProtocolPolicy where
    toXML = toXMLText

data PriceClass
    = PriceClass200
    | PriceClass100
    | PriceClassAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PriceClass where
    parser = takeLowerText >>= \case
        "priceclass_100" -> pure PriceClass100
        "priceclass_200" -> pure PriceClass200
        "priceclass_all" -> pure PriceClassAll
        e -> fromTextError $ "Failure parsing PriceClass from value: '" <> e
           <> "'. Accepted values: priceclass_100, priceclass_200, priceclass_all"

instance ToText PriceClass where
    toText = \case
        PriceClass100 -> "priceclass_100"
        PriceClass200 -> "priceclass_200"
        PriceClassAll -> "priceclass_all"

instance Hashable     PriceClass
instance ToByteString PriceClass
instance ToQuery      PriceClass
instance ToHeader     PriceClass

instance FromXML PriceClass where
    parseXML = parseXMLText "PriceClass"

instance ToXML PriceClass where
    toXML = toXMLText

data SSLSupportMethod
    = VIP
    | SNIOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SSLSupportMethod where
    parser = takeLowerText >>= \case
        "sni-only" -> pure SNIOnly
        "vip" -> pure VIP
        e -> fromTextError $ "Failure parsing SSLSupportMethod from value: '" <> e
           <> "'. Accepted values: sni-only, vip"

instance ToText SSLSupportMethod where
    toText = \case
        SNIOnly -> "sni-only"
        VIP -> "vip"

instance Hashable     SSLSupportMethod
instance ToByteString SSLSupportMethod
instance ToQuery      SSLSupportMethod
instance ToHeader     SSLSupportMethod

instance FromXML SSLSupportMethod where
    parseXML = parseXMLText "SSLSupportMethod"

instance ToXML SSLSupportMethod where
    toXML = toXMLText

data ViewerProtocolPolicy
    = HTTPSOnly
    | RedirectToHTTPS
    | AllowAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ViewerProtocolPolicy where
    parser = takeLowerText >>= \case
        "allow-all" -> pure AllowAll
        "https-only" -> pure HTTPSOnly
        "redirect-to-https" -> pure RedirectToHTTPS
        e -> fromTextError $ "Failure parsing ViewerProtocolPolicy from value: '" <> e
           <> "'. Accepted values: allow-all, https-only, redirect-to-https"

instance ToText ViewerProtocolPolicy where
    toText = \case
        AllowAll -> "allow-all"
        HTTPSOnly -> "https-only"
        RedirectToHTTPS -> "redirect-to-https"

instance Hashable     ViewerProtocolPolicy
instance ToByteString ViewerProtocolPolicy
instance ToQuery      ViewerProtocolPolicy
instance ToHeader     ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
    parseXML = parseXMLText "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
    toXML = toXMLText
