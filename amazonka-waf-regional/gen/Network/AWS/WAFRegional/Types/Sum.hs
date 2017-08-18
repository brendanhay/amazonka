{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.Sum where

import           Network.AWS.Prelude

data ChangeAction
    = Delete
    | Insert
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "insert" -> pure Insert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: delete, insert"

instance ToText ChangeAction where
    toText = \case
        Delete -> "DELETE"
        Insert -> "INSERT"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance ToJSON ChangeAction where
    toJSON = toJSONText

data ChangeTokenStatus
    = Insync
    | Pending
    | Provisioned
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChangeTokenStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        "provisioned" -> pure Provisioned
        e -> fromTextError $ "Failure parsing ChangeTokenStatus from value: '" <> e
           <> "'. Accepted values: insync, pending, provisioned"

instance ToText ChangeTokenStatus where
    toText = \case
        Insync -> "INSYNC"
        Pending -> "PENDING"
        Provisioned -> "PROVISIONED"

instance Hashable     ChangeTokenStatus
instance NFData       ChangeTokenStatus
instance ToByteString ChangeTokenStatus
instance ToQuery      ChangeTokenStatus
instance ToHeader     ChangeTokenStatus

instance FromJSON ChangeTokenStatus where
    parseJSON = parseJSONText "ChangeTokenStatus"

data ComparisonOperator
    = EQ'
    | GE
    | GT'
    | LE
    | LT'
    | NE
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "eq" -> pure EQ'
        "ge" -> pure GE
        "gt" -> pure GT'
        "le" -> pure LE
        "lt" -> pure LT'
        "ne" -> pure NE
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: eq, ge, gt, le, lt, ne"

instance ToText ComparisonOperator where
    toText = \case
        EQ' -> "EQ"
        GE -> "GE"
        GT' -> "GT"
        LE -> "LE"
        LT' -> "LT"
        NE -> "NE"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"

data IPSetDescriptorType
    = IPV4
    | IPV6
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText IPSetDescriptorType where
    parser = takeLowerText >>= \case
        "ipv4" -> pure IPV4
        "ipv6" -> pure IPV6
        e -> fromTextError $ "Failure parsing IPSetDescriptorType from value: '" <> e
           <> "'. Accepted values: ipv4, ipv6"

instance ToText IPSetDescriptorType where
    toText = \case
        IPV4 -> "IPV4"
        IPV6 -> "IPV6"

instance Hashable     IPSetDescriptorType
instance NFData       IPSetDescriptorType
instance ToByteString IPSetDescriptorType
instance ToQuery      IPSetDescriptorType
instance ToHeader     IPSetDescriptorType

instance ToJSON IPSetDescriptorType where
    toJSON = toJSONText

instance FromJSON IPSetDescriptorType where
    parseJSON = parseJSONText "IPSetDescriptorType"

data MatchFieldType
    = Body
    | Header
    | Method
    | QueryString
    | URI
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MatchFieldType where
    parser = takeLowerText >>= \case
        "body" -> pure Body
        "header" -> pure Header
        "method" -> pure Method
        "query_string" -> pure QueryString
        "uri" -> pure URI
        e -> fromTextError $ "Failure parsing MatchFieldType from value: '" <> e
           <> "'. Accepted values: body, header, method, query_string, uri"

instance ToText MatchFieldType where
    toText = \case
        Body -> "BODY"
        Header -> "HEADER"
        Method -> "METHOD"
        QueryString -> "QUERY_STRING"
        URI -> "URI"

instance Hashable     MatchFieldType
instance NFData       MatchFieldType
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PositionalConstraint where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "contains_word" -> pure ContainsWord
        "ends_with" -> pure EndsWith
        "exactly" -> pure Exactly
        "starts_with" -> pure StartsWith
        e -> fromTextError $ "Failure parsing PositionalConstraint from value: '" <> e
           <> "'. Accepted values: contains, contains_word, ends_with, exactly, starts_with"

instance ToText PositionalConstraint where
    toText = \case
        Contains -> "CONTAINS"
        ContainsWord -> "CONTAINS_WORD"
        EndsWith -> "ENDS_WITH"
        Exactly -> "EXACTLY"
        StartsWith -> "STARTS_WITH"

instance Hashable     PositionalConstraint
instance NFData       PositionalConstraint
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
    | SizeConstraint
    | SqlInjectionMatch
    | XSSMatch
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText PredicateType where
    parser = takeLowerText >>= \case
        "bytematch" -> pure ByteMatch
        "ipmatch" -> pure IPMatch
        "sizeconstraint" -> pure SizeConstraint
        "sqlinjectionmatch" -> pure SqlInjectionMatch
        "xssmatch" -> pure XSSMatch
        e -> fromTextError $ "Failure parsing PredicateType from value: '" <> e
           <> "'. Accepted values: bytematch, ipmatch, sizeconstraint, sqlinjectionmatch, xssmatch"

instance ToText PredicateType where
    toText = \case
        ByteMatch -> "ByteMatch"
        IPMatch -> "IPMatch"
        SizeConstraint -> "SizeConstraint"
        SqlInjectionMatch -> "SqlInjectionMatch"
        XSSMatch -> "XssMatch"

instance Hashable     PredicateType
instance NFData       PredicateType
instance ToByteString PredicateType
instance ToQuery      PredicateType
instance ToHeader     PredicateType

instance ToJSON PredicateType where
    toJSON = toJSONText

instance FromJSON PredicateType where
    parseJSON = parseJSONText "PredicateType"

data RateKey =
    IP
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RateKey where
    parser = takeLowerText >>= \case
        "ip" -> pure IP
        e -> fromTextError $ "Failure parsing RateKey from value: '" <> e
           <> "'. Accepted values: ip"

instance ToText RateKey where
    toText = \case
        IP -> "IP"

instance Hashable     RateKey
instance NFData       RateKey
instance ToByteString RateKey
instance ToQuery      RateKey
instance ToHeader     RateKey

instance ToJSON RateKey where
    toJSON = toJSONText

instance FromJSON RateKey where
    parseJSON = parseJSONText "RateKey"

data TextTransformation
    = CmdLine
    | CompressWhiteSpace
    | HTMLEntityDecode
    | Lowercase
    | None
    | URLDecode
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TextTransformation where
    parser = takeLowerText >>= \case
        "cmd_line" -> pure CmdLine
        "compress_white_space" -> pure CompressWhiteSpace
        "html_entity_decode" -> pure HTMLEntityDecode
        "lowercase" -> pure Lowercase
        "none" -> pure None
        "url_decode" -> pure URLDecode
        e -> fromTextError $ "Failure parsing TextTransformation from value: '" <> e
           <> "'. Accepted values: cmd_line, compress_white_space, html_entity_decode, lowercase, none, url_decode"

instance ToText TextTransformation where
    toText = \case
        CmdLine -> "CMD_LINE"
        CompressWhiteSpace -> "COMPRESS_WHITE_SPACE"
        HTMLEntityDecode -> "HTML_ENTITY_DECODE"
        Lowercase -> "LOWERCASE"
        None -> "NONE"
        URLDecode -> "URL_DECODE"

instance Hashable     TextTransformation
instance NFData       TextTransformation
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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText WafActionType where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "block" -> pure Block
        "count" -> pure Count
        e -> fromTextError $ "Failure parsing WafActionType from value: '" <> e
           <> "'. Accepted values: allow, block, count"

instance ToText WafActionType where
    toText = \case
        Allow -> "ALLOW"
        Block -> "BLOCK"
        Count -> "COUNT"

instance Hashable     WafActionType
instance NFData       WafActionType
instance ToByteString WafActionType
instance ToQuery      WafActionType
instance ToHeader     WafActionType

instance ToJSON WafActionType where
    toJSON = toJSONText

instance FromJSON WafActionType where
    parseJSON = parseJSONText "WafActionType"

data WafRuleType
    = RateBased
    | Regular
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText WafRuleType where
    parser = takeLowerText >>= \case
        "rate_based" -> pure RateBased
        "regular" -> pure Regular
        e -> fromTextError $ "Failure parsing WafRuleType from value: '" <> e
           <> "'. Accepted values: rate_based, regular"

instance ToText WafRuleType where
    toText = \case
        RateBased -> "RATE_BASED"
        Regular -> "REGULAR"

instance Hashable     WafRuleType
instance NFData       WafRuleType
instance ToByteString WafRuleType
instance ToQuery      WafRuleType
instance ToHeader     WafRuleType

instance ToJSON WafRuleType where
    toJSON = toJSONText

instance FromJSON WafRuleType where
    parseJSON = parseJSONText "WafRuleType"
