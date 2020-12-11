-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEStatus
  ( SSEStatus
      ( SSEStatus',
        SSESDisabled,
        SSESDisabling,
        SSESEnabled,
        SSESEnabling,
        SSESUpdating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SSEStatus = SSEStatus' Lude.Text
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

pattern SSESDisabled :: SSEStatus
pattern SSESDisabled = SSEStatus' "DISABLED"

pattern SSESDisabling :: SSEStatus
pattern SSESDisabling = SSEStatus' "DISABLING"

pattern SSESEnabled :: SSEStatus
pattern SSESEnabled = SSEStatus' "ENABLED"

pattern SSESEnabling :: SSEStatus
pattern SSESEnabling = SSEStatus' "ENABLING"

pattern SSESUpdating :: SSEStatus
pattern SSESUpdating = SSEStatus' "UPDATING"

{-# COMPLETE
  SSESDisabled,
  SSESDisabling,
  SSESEnabled,
  SSESEnabling,
  SSESUpdating,
  SSEStatus'
  #-}
