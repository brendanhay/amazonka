{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Transferable where

import Network.AWS.Prelude

-- | Whether the domain name can be transferred to Route 53.
--
--
-- Valid values:
--
--     * TRANSFERABLE    * The domain name can be transferred to Route 53.
--
--     * UNTRANSFERRABLE    * The domain name can't be transferred to Route 53.
--
--     * DONT_KNOW    * Reserved for future use.
data Transferable
  = DontKnow
  | Transferable
  | Untransferable
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

instance FromText Transferable where
  parser =
    takeLowerText >>= \case
      "dont_know" -> pure DontKnow
      "transferable" -> pure Transferable
      "untransferable" -> pure Untransferable
      e ->
        fromTextError $
          "Failure parsing Transferable from value: '" <> e
            <> "'. Accepted values: dont_know, transferable, untransferable"

instance ToText Transferable where
  toText = \case
    DontKnow -> "DONT_KNOW"
    Transferable -> "TRANSFERABLE"
    Untransferable -> "UNTRANSFERABLE"

instance Hashable Transferable

instance NFData Transferable

instance ToByteString Transferable

instance ToQuery Transferable

instance ToHeader Transferable

instance FromJSON Transferable where
  parseJSON = parseJSONText "Transferable"
