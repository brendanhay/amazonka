{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PidMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PidMode where

import Network.AWS.Prelude

data PidMode
  = PMHost
  | PMTask
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

instance FromText PidMode where
  parser =
    takeLowerText >>= \case
      "host" -> pure PMHost
      "task" -> pure PMTask
      e ->
        fromTextError $
          "Failure parsing PidMode from value: '" <> e
            <> "'. Accepted values: host, task"

instance ToText PidMode where
  toText = \case
    PMHost -> "host"
    PMTask -> "task"

instance Hashable PidMode

instance NFData PidMode

instance ToByteString PidMode

instance ToQuery PidMode

instance ToHeader PidMode

instance ToJSON PidMode where
  toJSON = toJSONText

instance FromJSON PidMode where
  parseJSON = parseJSONText "PidMode"
