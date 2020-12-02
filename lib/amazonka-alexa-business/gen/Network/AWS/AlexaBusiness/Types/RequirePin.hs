{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RequirePin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequirePin where

import Network.AWS.Prelude

data RequirePin
  = NO
  | Optional
  | Yes
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

instance FromText RequirePin where
  parser =
    takeLowerText >>= \case
      "no" -> pure NO
      "optional" -> pure Optional
      "yes" -> pure Yes
      e ->
        fromTextError $
          "Failure parsing RequirePin from value: '" <> e
            <> "'. Accepted values: no, optional, yes"

instance ToText RequirePin where
  toText = \case
    NO -> "NO"
    Optional -> "OPTIONAL"
    Yes -> "YES"

instance Hashable RequirePin

instance NFData RequirePin

instance ToByteString RequirePin

instance ToQuery RequirePin

instance ToHeader RequirePin

instance ToJSON RequirePin where
  toJSON = toJSONText

instance FromJSON RequirePin where
  parseJSON = parseJSONText "RequirePin"
