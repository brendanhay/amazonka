{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LastUpdateStatus where

import Network.AWS.Prelude

data LastUpdateStatus
  = LUSFailed
  | LUSInProgress
  | LUSSuccessful
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

instance FromText LastUpdateStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure LUSFailed
      "inprogress" -> pure LUSInProgress
      "successful" -> pure LUSSuccessful
      e ->
        fromTextError $
          "Failure parsing LastUpdateStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, successful"

instance ToText LastUpdateStatus where
  toText = \case
    LUSFailed -> "Failed"
    LUSInProgress -> "InProgress"
    LUSSuccessful -> "Successful"

instance Hashable LastUpdateStatus

instance NFData LastUpdateStatus

instance ToByteString LastUpdateStatus

instance ToQuery LastUpdateStatus

instance ToHeader LastUpdateStatus

instance FromJSON LastUpdateStatus where
  parseJSON = parseJSONText "LastUpdateStatus"
