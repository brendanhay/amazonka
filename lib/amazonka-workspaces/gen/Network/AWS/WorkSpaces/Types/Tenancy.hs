{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Tenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Tenancy where

import Network.AWS.Prelude

data Tenancy
  = Dedicated
  | Shared
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

instance FromText Tenancy where
  parser =
    takeLowerText >>= \case
      "dedicated" -> pure Dedicated
      "shared" -> pure Shared
      e ->
        fromTextError $
          "Failure parsing Tenancy from value: '" <> e
            <> "'. Accepted values: dedicated, shared"

instance ToText Tenancy where
  toText = \case
    Dedicated -> "DEDICATED"
    Shared -> "SHARED"

instance Hashable Tenancy

instance NFData Tenancy

instance ToByteString Tenancy

instance ToQuery Tenancy

instance ToHeader Tenancy

instance ToJSON Tenancy where
  toJSON = toJSONText

instance FromJSON Tenancy where
  parseJSON = parseJSONText "Tenancy"
