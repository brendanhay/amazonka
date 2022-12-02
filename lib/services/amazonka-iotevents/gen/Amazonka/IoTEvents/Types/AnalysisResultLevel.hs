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
-- Module      : Amazonka.IoTEvents.Types.AnalysisResultLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AnalysisResultLevel
  ( AnalysisResultLevel
      ( ..,
        AnalysisResultLevel_ERROR,
        AnalysisResultLevel_INFO,
        AnalysisResultLevel_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnalysisResultLevel = AnalysisResultLevel'
  { fromAnalysisResultLevel ::
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
