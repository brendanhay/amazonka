{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerClaimStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerClaimStatus where

import Network.AWS.Prelude

data GameServerClaimStatus = Claimed
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

instance FromText GameServerClaimStatus where
  parser =
    takeLowerText >>= \case
      "claimed" -> pure Claimed
      e ->
        fromTextError $
          "Failure parsing GameServerClaimStatus from value: '" <> e
            <> "'. Accepted values: claimed"

instance ToText GameServerClaimStatus where
  toText = \case
    Claimed -> "CLAIMED"

instance Hashable GameServerClaimStatus

instance NFData GameServerClaimStatus

instance ToByteString GameServerClaimStatus

instance ToQuery GameServerClaimStatus

instance ToHeader GameServerClaimStatus

instance FromJSON GameServerClaimStatus where
  parseJSON = parseJSONText "GameServerClaimStatus"
