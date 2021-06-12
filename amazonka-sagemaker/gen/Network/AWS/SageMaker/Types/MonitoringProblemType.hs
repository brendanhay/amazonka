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
-- Module      : Network.AWS.SageMaker.Types.MonitoringProblemType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringProblemType
  ( MonitoringProblemType
      ( ..,
        MonitoringProblemType_BinaryClassification,
        MonitoringProblemType_MulticlassClassification,
        MonitoringProblemType_Regression
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MonitoringProblemType = MonitoringProblemType'
  { fromMonitoringProblemType ::
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

pattern MonitoringProblemType_BinaryClassification :: MonitoringProblemType
pattern MonitoringProblemType_BinaryClassification = MonitoringProblemType' "BinaryClassification"

pattern MonitoringProblemType_MulticlassClassification :: MonitoringProblemType
pattern MonitoringProblemType_MulticlassClassification = MonitoringProblemType' "MulticlassClassification"

pattern MonitoringProblemType_Regression :: MonitoringProblemType
pattern MonitoringProblemType_Regression = MonitoringProblemType' "Regression"

{-# COMPLETE
  MonitoringProblemType_BinaryClassification,
  MonitoringProblemType_MulticlassClassification,
  MonitoringProblemType_Regression,
  MonitoringProblemType'
  #-}
