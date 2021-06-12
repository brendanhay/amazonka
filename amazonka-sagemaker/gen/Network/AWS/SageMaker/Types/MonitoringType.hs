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
-- Module      : Network.AWS.SageMaker.Types.MonitoringType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringType
  ( MonitoringType
      ( ..,
        MonitoringType_DataQuality,
        MonitoringType_ModelBias,
        MonitoringType_ModelExplainability,
        MonitoringType_ModelQuality
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MonitoringType = MonitoringType'
  { fromMonitoringType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MonitoringType_DataQuality :: MonitoringType
pattern MonitoringType_DataQuality = MonitoringType' "DataQuality"

pattern MonitoringType_ModelBias :: MonitoringType
pattern MonitoringType_ModelBias = MonitoringType' "ModelBias"

pattern MonitoringType_ModelExplainability :: MonitoringType
pattern MonitoringType_ModelExplainability = MonitoringType' "ModelExplainability"

pattern MonitoringType_ModelQuality :: MonitoringType
pattern MonitoringType_ModelQuality = MonitoringType' "ModelQuality"

{-# COMPLETE
  MonitoringType_DataQuality,
  MonitoringType_ModelBias,
  MonitoringType_ModelExplainability,
  MonitoringType_ModelQuality,
  MonitoringType'
  #-}
