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

import qualified Network.AWS.Prelude as Prelude

newtype MonitoringType = MonitoringType'
  { fromMonitoringType ::
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
