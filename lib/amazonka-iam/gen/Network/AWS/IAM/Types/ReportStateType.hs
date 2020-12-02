{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ReportStateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ReportStateType where

import Network.AWS.Prelude

data ReportStateType
  = RSTComplete
  | RSTInprogress
  | RSTStarted
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

instance FromText ReportStateType where
  parser =
    takeLowerText >>= \case
      "complete" -> pure RSTComplete
      "inprogress" -> pure RSTInprogress
      "started" -> pure RSTStarted
      e ->
        fromTextError $
          "Failure parsing ReportStateType from value: '" <> e
            <> "'. Accepted values: complete, inprogress, started"

instance ToText ReportStateType where
  toText = \case
    RSTComplete -> "COMPLETE"
    RSTInprogress -> "INPROGRESS"
    RSTStarted -> "STARTED"

instance Hashable ReportStateType

instance NFData ReportStateType

instance ToByteString ReportStateType

instance ToQuery ReportStateType

instance ToHeader ReportStateType

instance FromXML ReportStateType where
  parseXML = parseXMLText "ReportStateType"
