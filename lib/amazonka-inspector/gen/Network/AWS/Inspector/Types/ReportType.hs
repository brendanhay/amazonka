{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ReportType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ReportType where

import Network.AWS.Prelude

data ReportType
  = Finding
  | Full
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

instance FromText ReportType where
  parser =
    takeLowerText >>= \case
      "finding" -> pure Finding
      "full" -> pure Full
      e ->
        fromTextError $
          "Failure parsing ReportType from value: '" <> e
            <> "'. Accepted values: finding, full"

instance ToText ReportType where
  toText = \case
    Finding -> "FINDING"
    Full -> "FULL"

instance Hashable ReportType

instance NFData ReportType

instance ToByteString ReportType

instance ToQuery ReportType

instance ToHeader ReportType

instance ToJSON ReportType where
  toJSON = toJSONText
