-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemStatus
  ( OpsItemStatus
      ( OpsItemStatus',
        InProgress,
        Open,
        Resolved
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OpsItemStatus = OpsItemStatus' Lude.Text
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

pattern InProgress :: OpsItemStatus
pattern InProgress = OpsItemStatus' "InProgress"

pattern Open :: OpsItemStatus
pattern Open = OpsItemStatus' "Open"

pattern Resolved :: OpsItemStatus
pattern Resolved = OpsItemStatus' "Resolved"

{-# COMPLETE
  InProgress,
  Open,
  Resolved,
  OpsItemStatus'
  #-}
