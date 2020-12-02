{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ChangeTokenStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ChangeTokenStatus where

import Network.AWS.Prelude

data ChangeTokenStatus
  = Insync
  | Pending
  | Provisioned
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

instance FromText ChangeTokenStatus where
  parser =
    takeLowerText >>= \case
      "insync" -> pure Insync
      "pending" -> pure Pending
      "provisioned" -> pure Provisioned
      e ->
        fromTextError $
          "Failure parsing ChangeTokenStatus from value: '" <> e
            <> "'. Accepted values: insync, pending, provisioned"

instance ToText ChangeTokenStatus where
  toText = \case
    Insync -> "INSYNC"
    Pending -> "PENDING"
    Provisioned -> "PROVISIONED"

instance Hashable ChangeTokenStatus

instance NFData ChangeTokenStatus

instance ToByteString ChangeTokenStatus

instance ToQuery ChangeTokenStatus

instance ToHeader ChangeTokenStatus

instance FromJSON ChangeTokenStatus where
  parseJSON = parseJSONText "ChangeTokenStatus"
