{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.Sum where

import           Network.AWS.Prelude

data OperatorType
    = OperatorGE
    | OperatorEQ'
    | OperatorBetween
    | OperatorRefEQ
    | OperatorLE
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperatorType where
    parser = takeLowerText >>= \case
        "between" -> pure OperatorBetween
        "eq" -> pure OperatorEQ'
        "ge" -> pure OperatorGE
        "le" -> pure OperatorLE
        "ref_eq" -> pure OperatorRefEQ
        e -> fromTextError $ "Failure parsing OperatorType from value: '" <> e
           <> "'. Accepted values: between, eq, ge, le, ref_eq"

instance ToText OperatorType where
    toText = \case
        OperatorBetween -> "between"
        OperatorEQ' -> "eq"
        OperatorGE -> "ge"
        OperatorLE -> "le"
        OperatorRefEQ -> "ref_eq"

instance Hashable     OperatorType
instance ToByteString OperatorType
instance ToPath       OperatorType
instance ToQuery      OperatorType
instance ToHeader     OperatorType

instance ToJSON OperatorType where
    toJSON = toJSONText

data TaskStatus
    = Finished
    | False'
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText TaskStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "false" -> pure False'
        "finished" -> pure Finished
        e -> fromTextError $ "Failure parsing TaskStatus from value: '" <> e
           <> "'. Accepted values: failed, false, finished"

instance ToText TaskStatus where
    toText = \case
        Failed -> "failed"
        False' -> "false"
        Finished -> "finished"

instance Hashable     TaskStatus
instance ToByteString TaskStatus
instance ToPath       TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance ToJSON TaskStatus where
    toJSON = toJSONText
