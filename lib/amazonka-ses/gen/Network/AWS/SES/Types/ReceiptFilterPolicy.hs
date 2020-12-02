{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReceiptFilterPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReceiptFilterPolicy where

import Network.AWS.Prelude

data ReceiptFilterPolicy
  = Allow
  | Block
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

instance FromText ReceiptFilterPolicy where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "block" -> pure Block
      e ->
        fromTextError $
          "Failure parsing ReceiptFilterPolicy from value: '" <> e
            <> "'. Accepted values: allow, block"

instance ToText ReceiptFilterPolicy where
  toText = \case
    Allow -> "Allow"
    Block -> "Block"

instance Hashable ReceiptFilterPolicy

instance NFData ReceiptFilterPolicy

instance ToByteString ReceiptFilterPolicy

instance ToQuery ReceiptFilterPolicy

instance ToHeader ReceiptFilterPolicy

instance FromXML ReceiptFilterPolicy where
  parseXML = parseXMLText "ReceiptFilterPolicy"
