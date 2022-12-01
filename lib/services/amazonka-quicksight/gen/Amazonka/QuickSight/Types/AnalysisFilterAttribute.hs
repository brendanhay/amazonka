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
-- Module      : Amazonka.QuickSight.Types.AnalysisFilterAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisFilterAttribute
  ( AnalysisFilterAttribute
      ( ..,
        AnalysisFilterAttribute_ANALYSIS_NAME,
        AnalysisFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
        AnalysisFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
        AnalysisFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
        AnalysisFilterAttribute_QUICKSIGHT_OWNER,
        AnalysisFilterAttribute_QUICKSIGHT_USER,
        AnalysisFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AnalysisFilterAttribute = AnalysisFilterAttribute'
  { fromAnalysisFilterAttribute ::
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

pattern AnalysisFilterAttribute_ANALYSIS_NAME :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_ANALYSIS_NAME = AnalysisFilterAttribute' "ANALYSIS_NAME"

pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_OWNER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_OWNER = AnalysisFilterAttribute' "DIRECT_QUICKSIGHT_OWNER"

pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER = AnalysisFilterAttribute' "DIRECT_QUICKSIGHT_SOLE_OWNER"

pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER = AnalysisFilterAttribute' "DIRECT_QUICKSIGHT_VIEWER_OR_OWNER"

pattern AnalysisFilterAttribute_QUICKSIGHT_OWNER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_QUICKSIGHT_OWNER = AnalysisFilterAttribute' "QUICKSIGHT_OWNER"

pattern AnalysisFilterAttribute_QUICKSIGHT_USER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_QUICKSIGHT_USER = AnalysisFilterAttribute' "QUICKSIGHT_USER"

pattern AnalysisFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER :: AnalysisFilterAttribute
pattern AnalysisFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER = AnalysisFilterAttribute' "QUICKSIGHT_VIEWER_OR_OWNER"

{-# COMPLETE
  AnalysisFilterAttribute_ANALYSIS_NAME,
  AnalysisFilterAttribute_DIRECT_QUICKSIGHT_OWNER,
  AnalysisFilterAttribute_DIRECT_QUICKSIGHT_SOLE_OWNER,
  AnalysisFilterAttribute_DIRECT_QUICKSIGHT_VIEWER_OR_OWNER,
  AnalysisFilterAttribute_QUICKSIGHT_OWNER,
  AnalysisFilterAttribute_QUICKSIGHT_USER,
  AnalysisFilterAttribute_QUICKSIGHT_VIEWER_OR_OWNER,
  AnalysisFilterAttribute'
  #-}
