{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.ClientVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.ClientVersion where

import Network.AWS.Prelude

data ClientVersion
  = VD5_1
  | VD5_3
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

instance FromText ClientVersion where
  parser =
    takeLowerText >>= \case
      "5.1" -> pure VD5_1
      "5.3" -> pure VD5_3
      e ->
        fromTextError $
          "Failure parsing ClientVersion from value: '" <> e
            <> "'. Accepted values: 5.1, 5.3"

instance ToText ClientVersion where
  toText = \case
    VD5_1 -> "5.1"
    VD5_3 -> "5.3"

instance Hashable ClientVersion

instance NFData ClientVersion

instance ToByteString ClientVersion

instance ToQuery ClientVersion

instance ToHeader ClientVersion

instance ToJSON ClientVersion where
  toJSON = toJSONText
