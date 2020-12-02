{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageActionType where

import Network.AWS.Prelude

data ImageActionType = Expire
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

instance FromText ImageActionType where
  parser =
    takeLowerText >>= \case
      "expire" -> pure Expire
      e ->
        fromTextError $
          "Failure parsing ImageActionType from value: '" <> e
            <> "'. Accepted values: expire"

instance ToText ImageActionType where
  toText = \case
    Expire -> "EXPIRE"

instance Hashable ImageActionType

instance NFData ImageActionType

instance ToByteString ImageActionType

instance ToQuery ImageActionType

instance ToHeader ImageActionType

instance FromJSON ImageActionType where
  parseJSON = parseJSONText "ImageActionType"
