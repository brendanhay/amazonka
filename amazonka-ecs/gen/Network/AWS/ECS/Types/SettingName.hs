{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SettingName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SettingName
  ( SettingName
      ( ..,
        SettingName_AwsvpcTrunking,
        SettingName_ContainerInsights,
        SettingName_ContainerInstanceLongArnFormat,
        SettingName_ServiceLongArnFormat,
        SettingName_TaskLongArnFormat
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SettingName = SettingName'
  { fromSettingName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern SettingName_AwsvpcTrunking :: SettingName
pattern SettingName_AwsvpcTrunking = SettingName' "awsvpcTrunking"

pattern SettingName_ContainerInsights :: SettingName
pattern SettingName_ContainerInsights = SettingName' "containerInsights"

pattern SettingName_ContainerInstanceLongArnFormat :: SettingName
pattern SettingName_ContainerInstanceLongArnFormat = SettingName' "containerInstanceLongArnFormat"

pattern SettingName_ServiceLongArnFormat :: SettingName
pattern SettingName_ServiceLongArnFormat = SettingName' "serviceLongArnFormat"

pattern SettingName_TaskLongArnFormat :: SettingName
pattern SettingName_TaskLongArnFormat = SettingName' "taskLongArnFormat"

{-# COMPLETE
  SettingName_AwsvpcTrunking,
  SettingName_ContainerInsights,
  SettingName_ContainerInstanceLongArnFormat,
  SettingName_ServiceLongArnFormat,
  SettingName_TaskLongArnFormat,
  SettingName'
  #-}
