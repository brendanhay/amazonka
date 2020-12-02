{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.LambdaFunctionTimeoutType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.LambdaFunctionTimeoutType where

import Network.AWS.Prelude

data LambdaFunctionTimeoutType = LFTTStartToClose
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

instance FromText LambdaFunctionTimeoutType where
  parser =
    takeLowerText >>= \case
      "start_to_close" -> pure LFTTStartToClose
      e ->
        fromTextError $
          "Failure parsing LambdaFunctionTimeoutType from value: '" <> e
            <> "'. Accepted values: start_to_close"

instance ToText LambdaFunctionTimeoutType where
  toText = \case
    LFTTStartToClose -> "START_TO_CLOSE"

instance Hashable LambdaFunctionTimeoutType

instance NFData LambdaFunctionTimeoutType

instance ToByteString LambdaFunctionTimeoutType

instance ToQuery LambdaFunctionTimeoutType

instance ToHeader LambdaFunctionTimeoutType

instance FromJSON LambdaFunctionTimeoutType where
  parseJSON = parseJSONText "LambdaFunctionTimeoutType"
