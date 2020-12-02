{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.RefreshStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.RefreshStrategy where

import Network.AWS.Prelude

data RefreshStrategy = Rolling
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

instance FromText RefreshStrategy where
  parser =
    takeLowerText >>= \case
      "rolling" -> pure Rolling
      e ->
        fromTextError $
          "Failure parsing RefreshStrategy from value: '" <> e
            <> "'. Accepted values: rolling"

instance ToText RefreshStrategy where
  toText = \case
    Rolling -> "Rolling"

instance Hashable RefreshStrategy

instance NFData RefreshStrategy

instance ToByteString RefreshStrategy

instance ToQuery RefreshStrategy

instance ToHeader RefreshStrategy
