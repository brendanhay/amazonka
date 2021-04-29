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
-- Module      : Network.AWS.CostExplorer.Types.RecommendationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RecommendationTarget
  ( RecommendationTarget
      ( ..,
        RecommendationTarget_CROSS_INSTANCE_FAMILY,
        RecommendationTarget_SAME_INSTANCE_FAMILY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RecommendationTarget = RecommendationTarget'
  { fromRecommendationTarget ::
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

pattern RecommendationTarget_CROSS_INSTANCE_FAMILY :: RecommendationTarget
pattern RecommendationTarget_CROSS_INSTANCE_FAMILY = RecommendationTarget' "CROSS_INSTANCE_FAMILY"

pattern RecommendationTarget_SAME_INSTANCE_FAMILY :: RecommendationTarget
pattern RecommendationTarget_SAME_INSTANCE_FAMILY = RecommendationTarget' "SAME_INSTANCE_FAMILY"

{-# COMPLETE
  RecommendationTarget_CROSS_INSTANCE_FAMILY,
  RecommendationTarget_SAME_INSTANCE_FAMILY,
  RecommendationTarget'
  #-}
