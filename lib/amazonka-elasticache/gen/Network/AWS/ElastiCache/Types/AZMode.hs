{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AZMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AZMode where

import Network.AWS.Prelude

data AZMode
  = CrossAz
  | SingleAz
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

instance FromText AZMode where
  parser =
    takeLowerText >>= \case
      "cross-az" -> pure CrossAz
      "single-az" -> pure SingleAz
      e ->
        fromTextError $
          "Failure parsing AZMode from value: '" <> e
            <> "'. Accepted values: cross-az, single-az"

instance ToText AZMode where
  toText = \case
    CrossAz -> "cross-az"
    SingleAz -> "single-az"

instance Hashable AZMode

instance NFData AZMode

instance ToByteString AZMode

instance ToQuery AZMode

instance ToHeader AZMode
