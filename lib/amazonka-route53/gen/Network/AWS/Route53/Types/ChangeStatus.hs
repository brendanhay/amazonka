{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ChangeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeStatus where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ChangeStatus
  = Insync
  | Pending
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

instance FromText ChangeStatus where
  parser =
    takeLowerText >>= \case
      "insync" -> pure Insync
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing ChangeStatus from value: '" <> e
            <> "'. Accepted values: insync, pending"

instance ToText ChangeStatus where
  toText = \case
    Insync -> "INSYNC"
    Pending -> "PENDING"

instance Hashable ChangeStatus

instance NFData ChangeStatus

instance ToByteString ChangeStatus

instance ToQuery ChangeStatus

instance ToHeader ChangeStatus

instance FromXML ChangeStatus where
  parseXML = parseXMLText "ChangeStatus"
