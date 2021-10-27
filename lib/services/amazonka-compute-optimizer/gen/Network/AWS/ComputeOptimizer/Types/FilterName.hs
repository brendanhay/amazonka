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
-- Module      : Network.AWS.ComputeOptimizer.Types.FilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.FilterName
  ( FilterName
      ( ..,
        FilterName_Finding,
        FilterName_FindingReasonCodes,
        FilterName_RecommendationSourceType
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FilterName = FilterName'
  { fromFilterName ::
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

pattern FilterName_Finding :: FilterName
pattern FilterName_Finding = FilterName' "Finding"

pattern FilterName_FindingReasonCodes :: FilterName
pattern FilterName_FindingReasonCodes = FilterName' "FindingReasonCodes"

pattern FilterName_RecommendationSourceType :: FilterName
pattern FilterName_RecommendationSourceType = FilterName' "RecommendationSourceType"

{-# COMPLETE
  FilterName_Finding,
  FilterName_FindingReasonCodes,
  FilterName_RecommendationSourceType,
  FilterName'
  #-}
