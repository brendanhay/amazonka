{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tier where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Tier
  = TBulk
  | TExpedited
  | TStandard
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

instance FromText Tier where
  parser =
    takeLowerText >>= \case
      "bulk" -> pure TBulk
      "expedited" -> pure TExpedited
      "standard" -> pure TStandard
      e ->
        fromTextError $
          "Failure parsing Tier from value: '" <> e
            <> "'. Accepted values: bulk, expedited, standard"

instance ToText Tier where
  toText = \case
    TBulk -> "Bulk"
    TExpedited -> "Expedited"
    TStandard -> "Standard"

instance Hashable Tier

instance NFData Tier

instance ToByteString Tier

instance ToQuery Tier

instance ToHeader Tier

instance ToXML Tier where
  toXML = toXMLText
