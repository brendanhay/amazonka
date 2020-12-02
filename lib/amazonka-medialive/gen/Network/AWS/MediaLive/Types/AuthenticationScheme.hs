{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AuthenticationScheme
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AuthenticationScheme where

import Network.AWS.Prelude

-- | Authentication Scheme
data AuthenticationScheme
  = Akamai
  | Common
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

instance FromText AuthenticationScheme where
  parser =
    takeLowerText >>= \case
      "akamai" -> pure Akamai
      "common" -> pure Common
      e ->
        fromTextError $
          "Failure parsing AuthenticationScheme from value: '" <> e
            <> "'. Accepted values: akamai, common"

instance ToText AuthenticationScheme where
  toText = \case
    Akamai -> "AKAMAI"
    Common -> "COMMON"

instance Hashable AuthenticationScheme

instance NFData AuthenticationScheme

instance ToByteString AuthenticationScheme

instance ToQuery AuthenticationScheme

instance ToHeader AuthenticationScheme

instance ToJSON AuthenticationScheme where
  toJSON = toJSONText

instance FromJSON AuthenticationScheme where
  parseJSON = parseJSONText "AuthenticationScheme"
