{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RecordMarkerFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RecordMarkerFailedCause where

import Network.AWS.Prelude

data RecordMarkerFailedCause = OperationNotPermitted
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

instance FromText RecordMarkerFailedCause where
  parser =
    takeLowerText >>= \case
      "operation_not_permitted" -> pure OperationNotPermitted
      e ->
        fromTextError $
          "Failure parsing RecordMarkerFailedCause from value: '" <> e
            <> "'. Accepted values: operation_not_permitted"

instance ToText RecordMarkerFailedCause where
  toText = \case
    OperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable RecordMarkerFailedCause

instance NFData RecordMarkerFailedCause

instance ToByteString RecordMarkerFailedCause

instance ToQuery RecordMarkerFailedCause

instance ToHeader RecordMarkerFailedCause

instance FromJSON RecordMarkerFailedCause where
  parseJSON = parseJSONText "RecordMarkerFailedCause"
