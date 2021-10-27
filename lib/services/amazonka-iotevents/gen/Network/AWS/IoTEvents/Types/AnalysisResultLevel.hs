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
-- Module      : Network.AWS.IoTEvents.Types.AnalysisResultLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.AnalysisResultLevel
  ( AnalysisResultLevel
      ( ..,
        AnalysisResultLevel_ERROR,
        AnalysisResultLevel_INFO,
        AnalysisResultLevel_WARNING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AnalysisResultLevel = AnalysisResultLevel'
  { fromAnalysisResultLevel ::
      Core.Text
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

pattern AnalysisResultLevel_ERROR :: AnalysisResultLevel
pattern AnalysisResultLevel_ERROR = AnalysisResultLevel' "ERROR"

pattern AnalysisResultLevel_INFO :: AnalysisResultLevel
pattern AnalysisResultLevel_INFO = AnalysisResultLevel' "INFO"

pattern AnalysisResultLevel_WARNING :: AnalysisResultLevel
pattern AnalysisResultLevel_WARNING = AnalysisResultLevel' "WARNING"

{-# COMPLETE
  AnalysisResultLevel_ERROR,
  AnalysisResultLevel_INFO,
  AnalysisResultLevel_WARNING,
  AnalysisResultLevel'
  #-}
