{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types.SettingName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.SettingName
  ( SettingName
      ( ..,
        SettingName_AwsvpcTrunking,
        SettingName_ContainerInsights,
        SettingName_ContainerInstanceLongArnFormat,
        SettingName_FargateFIPSMode,
        SettingName_ServiceLongArnFormat,
        SettingName_TagResourceAuthorization,
        SettingName_TaskLongArnFormat
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SettingName = SettingName'
  { fromSettingName ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern SettingName_AwsvpcTrunking :: SettingName
pattern SettingName_AwsvpcTrunking = SettingName' "awsvpcTrunking"

pattern SettingName_ContainerInsights :: SettingName
pattern SettingName_ContainerInsights = SettingName' "containerInsights"

pattern SettingName_ContainerInstanceLongArnFormat :: SettingName
pattern SettingName_ContainerInstanceLongArnFormat = SettingName' "containerInstanceLongArnFormat"

pattern SettingName_FargateFIPSMode :: SettingName
pattern SettingName_FargateFIPSMode = SettingName' "fargateFIPSMode"

pattern SettingName_ServiceLongArnFormat :: SettingName
pattern SettingName_ServiceLongArnFormat = SettingName' "serviceLongArnFormat"

pattern SettingName_TagResourceAuthorization :: SettingName
pattern SettingName_TagResourceAuthorization = SettingName' "tagResourceAuthorization"

pattern SettingName_TaskLongArnFormat :: SettingName
pattern SettingName_TaskLongArnFormat = SettingName' "taskLongArnFormat"

{-# COMPLETE
  SettingName_AwsvpcTrunking,
  SettingName_ContainerInsights,
  SettingName_ContainerInstanceLongArnFormat,
  SettingName_FargateFIPSMode,
  SettingName_ServiceLongArnFormat,
  SettingName_TagResourceAuthorization,
  SettingName_TaskLongArnFormat,
  SettingName'
  #-}
