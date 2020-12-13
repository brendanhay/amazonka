{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ServiceLongARNFormat,
        TaskLongARNFormat,
        ContainerInstanceLongARNFormat,
        AWSvpcTrunking,
        ContainerInsights
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

pattern ServiceLongARNFormat :: SettingName
pattern ServiceLongARNFormat = SettingName' "serviceLongArnFormat"

pattern TaskLongARNFormat :: SettingName
pattern TaskLongARNFormat = SettingName' "taskLongArnFormat"

pattern ContainerInstanceLongARNFormat :: SettingName
pattern ContainerInstanceLongARNFormat = SettingName' "containerInstanceLongArnFormat"

pattern AWSvpcTrunking :: SettingName
pattern AWSvpcTrunking = SettingName' "awsvpcTrunking"

pattern ContainerInsights :: SettingName
pattern ContainerInsights = SettingName' "containerInsights"

{-# COMPLETE
  ServiceLongARNFormat,
  TaskLongARNFormat,
  ContainerInstanceLongARNFormat,
  AWSvpcTrunking,
  ContainerInsights,
  SettingName'
  #-}
