{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.OutpostMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.OutpostMode where

import Network.AWS.Prelude

data OutpostMode
  = CrossOutpost
  | SingleOutpost
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

instance FromText OutpostMode where
  parser =
    takeLowerText >>= \case
      "cross-outpost" -> pure CrossOutpost
      "single-outpost" -> pure SingleOutpost
      e ->
        fromTextError $
          "Failure parsing OutpostMode from value: '" <> e
            <> "'. Accepted values: cross-outpost, single-outpost"

instance ToText OutpostMode where
  toText = \case
    CrossOutpost -> "cross-outpost"
    SingleOutpost -> "single-outpost"

instance Hashable OutpostMode

instance NFData OutpostMode

instance ToByteString OutpostMode

instance ToQuery OutpostMode

instance ToHeader OutpostMode
