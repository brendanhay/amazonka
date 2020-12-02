{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelActivityTaskFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelActivityTaskFailedCause where

import Network.AWS.Prelude

data RequestCancelActivityTaskFailedCause
  = RCATFCActivityIdUnknown
  | RCATFCOperationNotPermitted
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

instance FromText RequestCancelActivityTaskFailedCause where
  parser =
    takeLowerText >>= \case
      "activity_id_unknown" -> pure RCATFCActivityIdUnknown
      "operation_not_permitted" -> pure RCATFCOperationNotPermitted
      e ->
        fromTextError $
          "Failure parsing RequestCancelActivityTaskFailedCause from value: '" <> e
            <> "'. Accepted values: activity_id_unknown, operation_not_permitted"

instance ToText RequestCancelActivityTaskFailedCause where
  toText = \case
    RCATFCActivityIdUnknown -> "ACTIVITY_ID_UNKNOWN"
    RCATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable RequestCancelActivityTaskFailedCause

instance NFData RequestCancelActivityTaskFailedCause

instance ToByteString RequestCancelActivityTaskFailedCause

instance ToQuery RequestCancelActivityTaskFailedCause

instance ToHeader RequestCancelActivityTaskFailedCause

instance FromJSON RequestCancelActivityTaskFailedCause where
  parseJSON = parseJSONText "RequestCancelActivityTaskFailedCause"
