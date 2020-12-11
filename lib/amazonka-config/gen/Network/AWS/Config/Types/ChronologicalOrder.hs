-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ChronologicalOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ChronologicalOrder
  ( ChronologicalOrder
      ( ChronologicalOrder',
        Forward,
        Reverse
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChronologicalOrder = ChronologicalOrder' Lude.Text
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

pattern Forward :: ChronologicalOrder
pattern Forward = ChronologicalOrder' "Forward"

pattern Reverse :: ChronologicalOrder
pattern Reverse = ChronologicalOrder' "Reverse"

{-# COMPLETE
  Forward,
  Reverse,
  ChronologicalOrder'
  #-}
