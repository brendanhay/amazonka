{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.JoinSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.JoinSource where

import Network.AWS.Prelude

data JoinSource
  = JSInput
  | JSNone
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

instance FromText JoinSource where
  parser =
    takeLowerText >>= \case
      "input" -> pure JSInput
      "none" -> pure JSNone
      e ->
        fromTextError $
          "Failure parsing JoinSource from value: '" <> e
            <> "'. Accepted values: input, none"

instance ToText JoinSource where
  toText = \case
    JSInput -> "Input"
    JSNone -> "None"

instance Hashable JoinSource

instance NFData JoinSource

instance ToByteString JoinSource

instance ToQuery JoinSource

instance ToHeader JoinSource

instance ToJSON JoinSource where
  toJSON = toJSONText

instance FromJSON JoinSource where
  parseJSON = parseJSONText "JoinSource"
