{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.QueryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryType
  ( QueryType
      ( QueryType',
        QueryTypeTagFilters10,
        QueryTypeCloudformationStack10,
        fromQueryType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype QueryType = QueryType' {fromQueryType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern QueryTypeTagFilters10 :: QueryType
pattern QueryTypeTagFilters10 = QueryType' "TAG_FILTERS_1_0"

pattern QueryTypeCloudformationStack10 :: QueryType
pattern QueryTypeCloudformationStack10 = QueryType' "CLOUDFORMATION_STACK_1_0"

{-# COMPLETE
  QueryTypeTagFilters10,
  QueryTypeCloudformationStack10,
  QueryType'
  #-}
