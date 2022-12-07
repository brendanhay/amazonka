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
-- Module      : Amazonka.Connect.Types.HierarchyGroupMatchType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyGroupMatchType
  ( HierarchyGroupMatchType
      ( ..,
        HierarchyGroupMatchType_EXACT,
        HierarchyGroupMatchType_WITH_CHILD_GROUPS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HierarchyGroupMatchType = HierarchyGroupMatchType'
  { fromHierarchyGroupMatchType ::
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

pattern HierarchyGroupMatchType_EXACT :: HierarchyGroupMatchType
pattern HierarchyGroupMatchType_EXACT = HierarchyGroupMatchType' "EXACT"

pattern HierarchyGroupMatchType_WITH_CHILD_GROUPS :: HierarchyGroupMatchType
pattern HierarchyGroupMatchType_WITH_CHILD_GROUPS = HierarchyGroupMatchType' "WITH_CHILD_GROUPS"

{-# COMPLETE
  HierarchyGroupMatchType_EXACT,
  HierarchyGroupMatchType_WITH_CHILD_GROUPS,
  HierarchyGroupMatchType'
  #-}
