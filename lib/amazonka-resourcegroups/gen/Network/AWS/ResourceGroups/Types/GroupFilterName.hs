{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupFilterName where

import Network.AWS.Prelude

data GroupFilterName
  = GFNConfigurationType
  | GFNResourceType
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

instance FromText GroupFilterName where
  parser =
    takeLowerText >>= \case
      "configuration-type" -> pure GFNConfigurationType
      "resource-type" -> pure GFNResourceType
      e ->
        fromTextError $
          "Failure parsing GroupFilterName from value: '" <> e
            <> "'. Accepted values: configuration-type, resource-type"

instance ToText GroupFilterName where
  toText = \case
    GFNConfigurationType -> "configuration-type"
    GFNResourceType -> "resource-type"

instance Hashable GroupFilterName

instance NFData GroupFilterName

instance ToByteString GroupFilterName

instance ToQuery GroupFilterName

instance ToHeader GroupFilterName

instance ToJSON GroupFilterName where
  toJSON = toJSONText
