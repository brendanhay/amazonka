{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum where

import Network.AWS.Prelude

data DedicatedTenancySupportEnum = DTSEEnabled
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

instance FromText DedicatedTenancySupportEnum where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure DTSEEnabled
      e ->
        fromTextError $
          "Failure parsing DedicatedTenancySupportEnum from value: '" <> e
            <> "'. Accepted values: enabled"

instance ToText DedicatedTenancySupportEnum where
  toText = \case
    DTSEEnabled -> "ENABLED"

instance Hashable DedicatedTenancySupportEnum

instance NFData DedicatedTenancySupportEnum

instance ToByteString DedicatedTenancySupportEnum

instance ToQuery DedicatedTenancySupportEnum

instance ToHeader DedicatedTenancySupportEnum

instance ToJSON DedicatedTenancySupportEnum where
  toJSON = toJSONText
