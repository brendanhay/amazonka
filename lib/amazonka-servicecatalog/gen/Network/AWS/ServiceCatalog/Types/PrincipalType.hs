{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PrincipalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PrincipalType where

import Network.AWS.Prelude

data PrincipalType = IAM
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

instance FromText PrincipalType where
  parser =
    takeLowerText >>= \case
      "iam" -> pure IAM
      e ->
        fromTextError $
          "Failure parsing PrincipalType from value: '" <> e
            <> "'. Accepted values: iam"

instance ToText PrincipalType where
  toText = \case
    IAM -> "IAM"

instance Hashable PrincipalType

instance NFData PrincipalType

instance ToByteString PrincipalType

instance ToQuery PrincipalType

instance ToHeader PrincipalType

instance ToJSON PrincipalType where
  toJSON = toJSONText

instance FromJSON PrincipalType where
  parseJSON = parseJSONText "PrincipalType"
