{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.SortOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.SortOrder
  ( SortOrder
      ( SortOrder',
        SortOrderAsc,
        SortOrderDsc,
        fromSortOrder
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The sort order specified in a listing condition. Possible values include the following:
--
--
--     * @asc@ - Present the information in ascending order (from A-Z).
--
--     * @dsc@ - Present the information in descending order (from Z-A).
newtype SortOrder = SortOrder' {fromSortOrder :: Core.Text}
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

pattern SortOrderAsc :: SortOrder
pattern SortOrderAsc = SortOrder' "asc"

pattern SortOrderDsc :: SortOrder
pattern SortOrderDsc = SortOrder' "dsc"

{-# COMPLETE
  SortOrderAsc,
  SortOrderDsc,
  SortOrder'
  #-}
