{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ExpirationModelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ExpirationModelType where

import Network.AWS.Prelude

data ExpirationModelType
  = KeyMaterialDoesNotExpire
  | KeyMaterialExpires
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

instance FromText ExpirationModelType where
  parser =
    takeLowerText >>= \case
      "key_material_does_not_expire" -> pure KeyMaterialDoesNotExpire
      "key_material_expires" -> pure KeyMaterialExpires
      e ->
        fromTextError $
          "Failure parsing ExpirationModelType from value: '" <> e
            <> "'. Accepted values: key_material_does_not_expire, key_material_expires"

instance ToText ExpirationModelType where
  toText = \case
    KeyMaterialDoesNotExpire -> "KEY_MATERIAL_DOES_NOT_EXPIRE"
    KeyMaterialExpires -> "KEY_MATERIAL_EXPIRES"

instance Hashable ExpirationModelType

instance NFData ExpirationModelType

instance ToByteString ExpirationModelType

instance ToQuery ExpirationModelType

instance ToHeader ExpirationModelType

instance ToJSON ExpirationModelType where
  toJSON = toJSONText

instance FromJSON ExpirationModelType where
  parseJSON = parseJSONText "ExpirationModelType"
