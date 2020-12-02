{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepStateChangeReasonCode where

import Network.AWS.Prelude

data StepStateChangeReasonCode = SSCRCNone
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

instance FromText StepStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "none" -> pure SSCRCNone
      e ->
        fromTextError $
          "Failure parsing StepStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: none"

instance ToText StepStateChangeReasonCode where
  toText = \case
    SSCRCNone -> "NONE"

instance Hashable StepStateChangeReasonCode

instance NFData StepStateChangeReasonCode

instance ToByteString StepStateChangeReasonCode

instance ToQuery StepStateChangeReasonCode

instance ToHeader StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode where
  parseJSON = parseJSONText "StepStateChangeReasonCode"
