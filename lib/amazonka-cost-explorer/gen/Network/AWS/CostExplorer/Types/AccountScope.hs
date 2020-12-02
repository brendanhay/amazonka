{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AccountScope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AccountScope where

import Network.AWS.Prelude

data AccountScope
  = Linked
  | Payer
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

instance FromText AccountScope where
  parser =
    takeLowerText >>= \case
      "linked" -> pure Linked
      "payer" -> pure Payer
      e ->
        fromTextError $
          "Failure parsing AccountScope from value: '" <> e
            <> "'. Accepted values: linked, payer"

instance ToText AccountScope where
  toText = \case
    Linked -> "LINKED"
    Payer -> "PAYER"

instance Hashable AccountScope

instance NFData AccountScope

instance ToByteString AccountScope

instance ToQuery AccountScope

instance ToHeader AccountScope

instance ToJSON AccountScope where
  toJSON = toJSONText

instance FromJSON AccountScope where
  parseJSON = parseJSONText "AccountScope"
