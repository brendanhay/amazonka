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

import qualified Network.AWS.Prelude as Prelude

newtype GroupDefinitionType = GroupDefinitionType'
  { fromGroupDefinitionType ::
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
