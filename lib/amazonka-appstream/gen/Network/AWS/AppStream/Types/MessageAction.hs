{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.MessageAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.MessageAction where

import Network.AWS.Prelude

data MessageAction
  = Resend
  | Suppress
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

instance FromText MessageAction where
  parser =
    takeLowerText >>= \case
      "resend" -> pure Resend
      "suppress" -> pure Suppress
      e ->
        fromTextError $
          "Failure parsing MessageAction from value: '" <> e
            <> "'. Accepted values: resend, suppress"

instance ToText MessageAction where
  toText = \case
    Resend -> "RESEND"
    Suppress -> "SUPPRESS"

instance Hashable MessageAction

instance NFData MessageAction

instance ToByteString MessageAction

instance ToQuery MessageAction

instance ToHeader MessageAction

instance ToJSON MessageAction where
  toJSON = toJSONText
