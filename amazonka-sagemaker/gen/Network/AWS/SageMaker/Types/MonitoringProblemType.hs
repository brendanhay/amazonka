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

import qualified Network.AWS.Prelude as Prelude

newtype MonitoringProblemType = MonitoringProblemType'
  { fromMonitoringProblemType ::
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
