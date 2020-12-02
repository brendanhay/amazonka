{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyTemplateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyTemplateName where

import Network.AWS.Prelude

data PolicyTemplateName = BlankPolicy
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

instance FromText PolicyTemplateName where
  parser =
    takeLowerText >>= \case
      "blank_policy" -> pure BlankPolicy
      e ->
        fromTextError $
          "Failure parsing PolicyTemplateName from value: '" <> e
            <> "'. Accepted values: blank_policy"

instance ToText PolicyTemplateName where
  toText = \case
    BlankPolicy -> "BLANK_POLICY"

instance Hashable PolicyTemplateName

instance NFData PolicyTemplateName

instance ToByteString PolicyTemplateName

instance ToQuery PolicyTemplateName

instance ToHeader PolicyTemplateName

instance ToJSON PolicyTemplateName where
  toJSON = toJSONText

instance FromJSON PolicyTemplateName where
  parseJSON = parseJSONText "PolicyTemplateName"
