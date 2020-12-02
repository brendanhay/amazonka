{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ProcessBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ProcessBehavior where

import Network.AWS.Prelude

data ProcessBehavior
  = Build
  | Save
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

instance FromText ProcessBehavior where
  parser =
    takeLowerText >>= \case
      "build" -> pure Build
      "save" -> pure Save
      e ->
        fromTextError $
          "Failure parsing ProcessBehavior from value: '" <> e
            <> "'. Accepted values: build, save"

instance ToText ProcessBehavior where
  toText = \case
    Build -> "BUILD"
    Save -> "SAVE"

instance Hashable ProcessBehavior

instance NFData ProcessBehavior

instance ToByteString ProcessBehavior

instance ToQuery ProcessBehavior

instance ToHeader ProcessBehavior

instance ToJSON ProcessBehavior where
  toJSON = toJSONText
