{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RenewalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RenewalType where

import Network.AWS.Prelude

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
data RenewalType
  = AutoRenew
  | Expire
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

instance FromText RenewalType where
  parser =
    takeLowerText >>= \case
      "auto_renew" -> pure AutoRenew
      "expire" -> pure Expire
      e ->
        fromTextError $
          "Failure parsing RenewalType from value: '" <> e
            <> "'. Accepted values: auto_renew, expire"

instance ToText RenewalType where
  toText = \case
    AutoRenew -> "AUTO_RENEW"
    Expire -> "EXPIRE"

instance Hashable RenewalType

instance NFData RenewalType

instance ToByteString RenewalType

instance ToQuery RenewalType

instance ToHeader RenewalType

instance ToJSON RenewalType where
  toJSON = toJSONText

instance FromJSON RenewalType where
  parseJSON = parseJSONText "RenewalType"
