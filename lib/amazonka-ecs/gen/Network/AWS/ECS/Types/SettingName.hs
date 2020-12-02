{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SettingName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SettingName where

import Network.AWS.Prelude

data SettingName
  = SNAWSvpcTrunking
  | SNContainerInsights
  | SNContainerInstanceLongARNFormat
  | SNServiceLongARNFormat
  | SNTaskLongARNFormat
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

instance FromText SettingName where
  parser =
    takeLowerText >>= \case
      "awsvpctrunking" -> pure SNAWSvpcTrunking
      "containerinsights" -> pure SNContainerInsights
      "containerinstancelongarnformat" -> pure SNContainerInstanceLongARNFormat
      "servicelongarnformat" -> pure SNServiceLongARNFormat
      "tasklongarnformat" -> pure SNTaskLongARNFormat
      e ->
        fromTextError $
          "Failure parsing SettingName from value: '" <> e
            <> "'. Accepted values: awsvpctrunking, containerinsights, containerinstancelongarnformat, servicelongarnformat, tasklongarnformat"

instance ToText SettingName where
  toText = \case
    SNAWSvpcTrunking -> "awsvpcTrunking"
    SNContainerInsights -> "containerInsights"
    SNContainerInstanceLongARNFormat -> "containerInstanceLongArnFormat"
    SNServiceLongARNFormat -> "serviceLongArnFormat"
    SNTaskLongARNFormat -> "taskLongArnFormat"

instance Hashable SettingName

instance NFData SettingName

instance ToByteString SettingName

instance ToQuery SettingName

instance ToHeader SettingName

instance ToJSON SettingName where
  toJSON = toJSONText

instance FromJSON SettingName where
  parseJSON = parseJSONText "SettingName"
