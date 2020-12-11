-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SettingName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SettingName
  ( SettingName
      ( SettingName',
        SNAWSvpcTrunking,
        SNContainerInsights,
        SNContainerInstanceLongARNFormat,
        SNServiceLongARNFormat,
        SNTaskLongARNFormat
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SettingName = SettingName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SNAWSvpcTrunking :: SettingName
pattern SNAWSvpcTrunking = SettingName' "awsvpcTrunking"

pattern SNContainerInsights :: SettingName
pattern SNContainerInsights = SettingName' "containerInsights"

pattern SNContainerInstanceLongARNFormat :: SettingName
pattern SNContainerInstanceLongARNFormat = SettingName' "containerInstanceLongArnFormat"

pattern SNServiceLongARNFormat :: SettingName
pattern SNServiceLongARNFormat = SettingName' "serviceLongArnFormat"

pattern SNTaskLongARNFormat :: SettingName
pattern SNTaskLongARNFormat = SettingName' "taskLongArnFormat"

{-# COMPLETE
  SNAWSvpcTrunking,
  SNContainerInsights,
  SNContainerInstanceLongARNFormat,
  SNServiceLongARNFormat,
  SNTaskLongARNFormat,
  SettingName'
  #-}
