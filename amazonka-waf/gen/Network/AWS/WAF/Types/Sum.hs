{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.Sum where

import           Network.AWS.Prelude

data ChangeAction
    = Delete
    | Insert
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "insert" -> pure Insert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: DELETE, INSERT"

instance ToText ChangeAction where
    toText = \case
        Delete -> "DELETE"
        Insert -> "INSERT"

instance Hashable     ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance ToJSON ChangeAction where
    toJSON = toJSONText

data ChangeTokenStatus
    = Insync
    | Pending
    | Provisioned
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ChangeTokenStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        "provisioned" -> pure Provisioned
        e -> fromTextError $ "Failure parsing ChangeTokenStatus from value: '" <> e
           <> "'. Accepted values: INSYNC, PENDING, PROVISIONED"

instance ToText ChangeTokenStatus where
    toText = \case
        Insync -> "INSYNC"
        Pending -> "PENDING"
        Provisioned -> "PROVISIONED"

instance Hashable     ChangeTokenStatus
instance ToByteString ChangeTokenStatus
instance ToQuery      ChangeTokenStatus
instance ToHeader     ChangeTokenStatus

instance FromJSON ChangeTokenStatus where
    parseJSON = parseJSONText "ChangeTokenStatus"

data IPSetDescriptorType =
    IPV4
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText IPSetDescriptorType where
    parser = takeLowerText >>= \case
        "ipv4" -> pure IPV4
        e -> fromTextError $ "Failure parsing IPSetDescriptorType from value: '" <> e
           <> "'. Accepted values: IPV4"

instance ToText IPSetDescriptorType where
    toText = \case
        IPV4 -> "IPV4"

instance Hashable     IPSetDescriptorType
instance ToByteString IPSetDescriptorType
instance ToQuery      IPSetDescriptorType
instance ToHeader     IPSetDescriptorType

instance ToJSON IPSetDescriptorType where
    toJSON = toJSONText

instance FromJSON IPSetDescriptorType where
    parseJSON = parseJSONText "IPSetDescriptorType"

data MatchFieldType
    = Header
    | Method
    | QueryString
    | URI
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MatchFieldType where
    parser = takeLowerText >>= \case
        "header" -> pure Header
        "method" -> pure Method
        "query_string" -> pure QueryString
        "uri" -> pure URI
        e -> fromTextError $ "Failure parsing MatchFieldType from value: '" <> e
           <> "'. Accepted values: HEADER, METHOD, QUERY_STRING, URI"

instance ToText MatchFieldType where
    toText = \case
        Header -> "HEADER"
        Method -> "METHOD"
        QueryString -> "QUERY_STRING"
        URI -> "URI"

instance Hashable     MatchFieldType
instance ToByteString MatchFieldType
instance ToQuery      MatchFieldType
instance ToHeader     MatchFieldType

instance ToJSON MatchFieldType where
    toJSON = toJSONText

instance FromJSON MatchFieldType where
    parseJSON = parseJSONText "MatchFieldType"

data PositionalConstraint
    = Contains
    | ContainsWord
    | EndsWith
    | Exactly
    | StartsWith
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PositionalConstraint where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "contains_word" -> pure ContainsWord
        "ends_with" -> pure EndsWith
        "exactly" -> pure Exactly
        "starts_with" -> pure StartsWith
        e -> fromTextError $ "Failure parsing PositionalConstraint from value: '" <> e
           <> "'. Accepted values: CONTAINS, CONTAINS_WORD, ENDS_WITH, EXACTLY, STARTS_WITH"

instance ToText PositionalConstraint where
    toText = \case
        Contains -> "CONTAINS"
        ContainsWord -> "CONTAINS_WORD"
        EndsWith -> "ENDS_WITH"
        Exactly -> "EXACTLY"
        StartsWith -> "STARTS_WITH"

instance Hashable     PositionalConstraint
instance ToByteString PositionalConstraint
instance ToQuery      PositionalConstraint
instance ToHeader     PositionalConstraint

instance ToJSON PositionalConstraint where
    toJSON = toJSONText

instance FromJSON PositionalConstraint where
    parseJSON = parseJSONText "PositionalConstraint"

data PredicateType
    = ByteMatch
    | IPMatch
    | SqlInjectionMatch
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PredicateType where
    parser = takeLowerText >>= \case
        "bytematch" -> pure ByteMatch
        "ipmatch" -> pure IPMatch
        "sqlinjectionmatch" -> pure SqlInjectionMatch
        e -> fromTextError $ "Failure parsing PredicateType from value: '" <> e
           <> "'. Accepted values: ByteMatch, IPMatch, SqlInjectionMatch"

instance ToText PredicateType where
    toText = \case
        ByteMatch -> "ByteMatch"
        IPMatch -> "IPMatch"
        SqlInjectionMatch -> "SqlInjectionMatch"

instance Hashable     PredicateType
instance ToByteString PredicateType
instance ToQuery      PredicateType
instance ToHeader     PredicateType

instance ToJSON PredicateType where
    toJSON = toJSONText

instance FromJSON PredicateType where
    parseJSON = parseJSONText "PredicateType"

data TextTransformation
    = CmdLine
    | CompressWhiteSpace
    | HTMLEntityDecode
    | Lowercase
    | None
    | URLDecode
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TextTransformation where
    parser = takeLowerText >>= \case
        "cmd_line" -> pure CmdLine
        "compress_white_space" -> pure CompressWhiteSpace
        "html_entity_decode" -> pure HTMLEntityDecode
        "lowercase" -> pure Lowercase
        "none" -> pure None
        "url_decode" -> pure URLDecode
        e -> fromTextError $ "Failure parsing TextTransformation from value: '" <> e
           <> "'. Accepted values: CMD_LINE, COMPRESS_WHITE_SPACE, HTML_ENTITY_DECODE, LOWERCASE, NONE, URL_DECODE"

instance ToText TextTransformation where
    toText = \case
        CmdLine -> "CMD_LINE"
        CompressWhiteSpace -> "COMPRESS_WHITE_SPACE"
        HTMLEntityDecode -> "HTML_ENTITY_DECODE"
        Lowercase -> "LOWERCASE"
        None -> "NONE"
        URLDecode -> "URL_DECODE"

instance Hashable     TextTransformation
instance ToByteString TextTransformation
instance ToQuery      TextTransformation
instance ToHeader     TextTransformation

instance ToJSON TextTransformation where
    toJSON = toJSONText

instance FromJSON TextTransformation where
    parseJSON = parseJSONText "TextTransformation"

data WafActionType
    = Allow
    | Block
    | Count
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText WafActionType where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "block" -> pure Block
        "count" -> pure Count
        e -> fromTextError $ "Failure parsing WafActionType from value: '" <> e
           <> "'. Accepted values: ALLOW, BLOCK, COUNT"

instance ToText WafActionType where
    toText = \case
        Allow -> "ALLOW"
        Block -> "BLOCK"
        Count -> "COUNT"

instance Hashable     WafActionType
instance ToByteString WafActionType
instance ToQuery      WafActionType
instance ToHeader     WafActionType

instance ToJSON WafActionType where
    toJSON = toJSONText

instance FromJSON WafActionType where
    parseJSON = parseJSONText "WafActionType"
