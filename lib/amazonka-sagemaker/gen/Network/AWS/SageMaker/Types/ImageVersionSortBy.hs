{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionSortBy where

import Network.AWS.Prelude

data ImageVersionSortBy
  = IVSBCreationTime
  | IVSBLastModifiedTime
  | IVSBVersion
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

instance FromText ImageVersionSortBy where
  parser =
    takeLowerText >>= \case
      "creation_time" -> pure IVSBCreationTime
      "last_modified_time" -> pure IVSBLastModifiedTime
      "version" -> pure IVSBVersion
      e ->
        fromTextError $
          "Failure parsing ImageVersionSortBy from value: '" <> e
            <> "'. Accepted values: creation_time, last_modified_time, version"

instance ToText ImageVersionSortBy where
  toText = \case
    IVSBCreationTime -> "CREATION_TIME"
    IVSBLastModifiedTime -> "LAST_MODIFIED_TIME"
    IVSBVersion -> "VERSION"

instance Hashable ImageVersionSortBy

instance NFData ImageVersionSortBy

instance ToByteString ImageVersionSortBy

instance ToQuery ImageVersionSortBy

instance ToHeader ImageVersionSortBy

instance ToJSON ImageVersionSortBy where
  toJSON = toJSONText
