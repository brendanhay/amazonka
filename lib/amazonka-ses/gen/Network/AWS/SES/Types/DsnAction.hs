-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DsnAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DsnAction
  ( DsnAction
      ( DsnAction',
        DADelayed,
        DADelivered,
        DAExpanded,
        DAFailed,
        DARelayed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DsnAction = DsnAction' Lude.Text
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

pattern DADelayed :: DsnAction
pattern DADelayed = DsnAction' "delayed"

pattern DADelivered :: DsnAction
pattern DADelivered = DsnAction' "delivered"

pattern DAExpanded :: DsnAction
pattern DAExpanded = DsnAction' "expanded"

pattern DAFailed :: DsnAction
pattern DAFailed = DsnAction' "failed"

pattern DARelayed :: DsnAction
pattern DARelayed = DsnAction' "relayed"

{-# COMPLETE
  DADelayed,
  DADelivered,
  DAExpanded,
  DAFailed,
  DARelayed,
  DsnAction'
  #-}
