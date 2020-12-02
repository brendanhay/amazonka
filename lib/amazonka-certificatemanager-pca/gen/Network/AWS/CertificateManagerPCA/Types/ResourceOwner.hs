{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ResourceOwner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ResourceOwner where

import Network.AWS.Prelude

data ResourceOwner
  = OtherAccounts
  | Self
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

instance FromText ResourceOwner where
  parser =
    takeLowerText >>= \case
      "other_accounts" -> pure OtherAccounts
      "self" -> pure Self
      e ->
        fromTextError $
          "Failure parsing ResourceOwner from value: '" <> e
            <> "'. Accepted values: other_accounts, self"

instance ToText ResourceOwner where
  toText = \case
    OtherAccounts -> "OTHER_ACCOUNTS"
    Self -> "SELF"

instance Hashable ResourceOwner

instance NFData ResourceOwner

instance ToByteString ResourceOwner

instance ToQuery ResourceOwner

instance ToHeader ResourceOwner

instance ToJSON ResourceOwner where
  toJSON = toJSONText
