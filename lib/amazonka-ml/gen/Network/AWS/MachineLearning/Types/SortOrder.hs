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
        Asc,
        Dsc
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The sort order specified in a listing condition. Possible values include the following:
--
--
--     * @asc@ - Present the information in ascending order (from A-Z).
--
--     * @dsc@ - Present the information in descending order (from Z-A).
newtype SortOrder = SortOrder' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Asc :: SortOrder
pattern Asc = SortOrder' "asc"

pattern Dsc :: SortOrder
pattern Dsc = SortOrder' "dsc"

{-# COMPLETE
  Asc,
  Dsc,
  SortOrder'
  #-}
