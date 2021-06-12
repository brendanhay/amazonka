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
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinitionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.GroupDefinitionType
  ( GroupDefinitionType
      ( ..,
        GroupDefinitionType_COST_CATEGORY,
        GroupDefinitionType_DIMENSION,
        GroupDefinitionType_TAG
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GroupDefinitionType = GroupDefinitionType'
  { fromGroupDefinitionType ::
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

pattern GroupDefinitionType_COST_CATEGORY :: GroupDefinitionType
pattern GroupDefinitionType_COST_CATEGORY = GroupDefinitionType' "COST_CATEGORY"

pattern GroupDefinitionType_DIMENSION :: GroupDefinitionType
pattern GroupDefinitionType_DIMENSION = GroupDefinitionType' "DIMENSION"

pattern GroupDefinitionType_TAG :: GroupDefinitionType
pattern GroupDefinitionType_TAG = GroupDefinitionType' "TAG"

{-# COMPLETE
  GroupDefinitionType_COST_CATEGORY,
  GroupDefinitionType_DIMENSION,
  GroupDefinitionType_TAG,
  GroupDefinitionType'
  #-}
