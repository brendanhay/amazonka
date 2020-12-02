{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantPropertyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantPropertyType where

import Network.AWS.Prelude

data VariantPropertyType
  = DataCaptureConfig
  | DesiredInstanceCount
  | DesiredWeight
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

instance FromText VariantPropertyType where
  parser =
    takeLowerText >>= \case
      "datacaptureconfig" -> pure DataCaptureConfig
      "desiredinstancecount" -> pure DesiredInstanceCount
      "desiredweight" -> pure DesiredWeight
      e ->
        fromTextError $
          "Failure parsing VariantPropertyType from value: '" <> e
            <> "'. Accepted values: datacaptureconfig, desiredinstancecount, desiredweight"

instance ToText VariantPropertyType where
  toText = \case
    DataCaptureConfig -> "DataCaptureConfig"
    DesiredInstanceCount -> "DesiredInstanceCount"
    DesiredWeight -> "DesiredWeight"

instance Hashable VariantPropertyType

instance NFData VariantPropertyType

instance ToByteString VariantPropertyType

instance ToQuery VariantPropertyType

instance ToHeader VariantPropertyType

instance ToJSON VariantPropertyType where
  toJSON = toJSONText
