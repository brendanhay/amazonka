-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OrderEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OrderEnum
  ( OrderEnum
      ( OrderEnum',
        Ascending,
        Descending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrderEnum = OrderEnum' Lude.Text
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

pattern Ascending :: OrderEnum
pattern Ascending = OrderEnum' "ascending"

pattern Descending :: OrderEnum
pattern Descending = OrderEnum' "descending"

{-# COMPLETE
  Ascending,
  Descending,
  OrderEnum'
  #-}
