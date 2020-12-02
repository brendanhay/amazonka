{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobAbortCriteriaFailureType where

import Network.AWS.Prelude

data AWSJobAbortCriteriaFailureType
  = AJACFTAll
  | AJACFTFailed
  | AJACFTRejected
  | AJACFTTimedOut
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText AWSJobAbortCriteriaFailureType where
  parser =
    takeLowerText >>= \case
      "all" -> pure AJACFTAll
      "failed" -> pure AJACFTFailed
      "rejected" -> pure AJACFTRejected
      "timed_out" -> pure AJACFTTimedOut
      e ->
        fromTextError $
          "Failure parsing AWSJobAbortCriteriaFailureType from value: '" <> e
            <> "'. Accepted values: all, failed, rejected, timed_out"

instance ToText AWSJobAbortCriteriaFailureType where
  toText = \case
    AJACFTAll -> "ALL"
    AJACFTFailed -> "FAILED"
    AJACFTRejected -> "REJECTED"
    AJACFTTimedOut -> "TIMED_OUT"

instance Hashable AWSJobAbortCriteriaFailureType

instance NFData AWSJobAbortCriteriaFailureType

instance ToByteString AWSJobAbortCriteriaFailureType

instance ToQuery AWSJobAbortCriteriaFailureType

instance ToHeader AWSJobAbortCriteriaFailureType

instance ToJSON AWSJobAbortCriteriaFailureType where
  toJSON = toJSONText
