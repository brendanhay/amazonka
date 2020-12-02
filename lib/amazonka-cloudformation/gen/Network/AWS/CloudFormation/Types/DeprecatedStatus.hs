{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.DeprecatedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.DeprecatedStatus where

import Network.AWS.Prelude

data DeprecatedStatus
  = Deprecated
  | Live
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

instance FromText DeprecatedStatus where
  parser =
    takeLowerText >>= \case
      "deprecated" -> pure Deprecated
      "live" -> pure Live
      e ->
        fromTextError $
          "Failure parsing DeprecatedStatus from value: '" <> e
            <> "'. Accepted values: deprecated, live"

instance ToText DeprecatedStatus where
  toText = \case
    Deprecated -> "DEPRECATED"
    Live -> "LIVE"

instance Hashable DeprecatedStatus

instance NFData DeprecatedStatus

instance ToByteString DeprecatedStatus

instance ToQuery DeprecatedStatus

instance ToHeader DeprecatedStatus

instance FromXML DeprecatedStatus where
  parseXML = parseXMLText "DeprecatedStatus"
