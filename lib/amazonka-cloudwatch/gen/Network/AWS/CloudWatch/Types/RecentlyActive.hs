{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.RecentlyActive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.RecentlyActive where

import Network.AWS.Prelude

data RecentlyActive = PT3H
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

instance FromText RecentlyActive where
  parser =
    takeLowerText >>= \case
      "pt3h" -> pure PT3H
      e ->
        fromTextError $
          "Failure parsing RecentlyActive from value: '" <> e
            <> "'. Accepted values: pt3h"

instance ToText RecentlyActive where
  toText = \case
    PT3H -> "PT3H"

instance Hashable RecentlyActive

instance NFData RecentlyActive

instance ToByteString RecentlyActive

instance ToQuery RecentlyActive

instance ToHeader RecentlyActive
