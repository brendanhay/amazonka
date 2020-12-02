{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductSource where

import Network.AWS.Prelude

data ProductSource = PSAccount
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

instance FromText ProductSource where
  parser =
    takeLowerText >>= \case
      "account" -> pure PSAccount
      e ->
        fromTextError $
          "Failure parsing ProductSource from value: '" <> e
            <> "'. Accepted values: account"

instance ToText ProductSource where
  toText = \case
    PSAccount -> "ACCOUNT"

instance Hashable ProductSource

instance NFData ProductSource

instance ToByteString ProductSource

instance ToQuery ProductSource

instance ToHeader ProductSource

instance ToJSON ProductSource where
  toJSON = toJSONText
