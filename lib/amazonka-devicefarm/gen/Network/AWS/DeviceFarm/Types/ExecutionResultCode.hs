{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResultCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionResultCode where

import Network.AWS.Prelude

data ExecutionResultCode
  = ParsingFailed
  | VPCEndpointSetupFailed
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

instance FromText ExecutionResultCode where
  parser =
    takeLowerText >>= \case
      "parsing_failed" -> pure ParsingFailed
      "vpc_endpoint_setup_failed" -> pure VPCEndpointSetupFailed
      e ->
        fromTextError $
          "Failure parsing ExecutionResultCode from value: '" <> e
            <> "'. Accepted values: parsing_failed, vpc_endpoint_setup_failed"

instance ToText ExecutionResultCode where
  toText = \case
    ParsingFailed -> "PARSING_FAILED"
    VPCEndpointSetupFailed -> "VPC_ENDPOINT_SETUP_FAILED"

instance Hashable ExecutionResultCode

instance NFData ExecutionResultCode

instance ToByteString ExecutionResultCode

instance ToQuery ExecutionResultCode

instance ToHeader ExecutionResultCode

instance FromJSON ExecutionResultCode where
  parseJSON = parseJSONText "ExecutionResultCode"
