-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveStatus
  ( TimeToLiveStatus
      ( TimeToLiveStatus',
        Disabled,
        Disabling,
        Enabled,
        Enabling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TimeToLiveStatus = TimeToLiveStatus' Lude.Text
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

pattern Disabled :: TimeToLiveStatus
pattern Disabled = TimeToLiveStatus' "DISABLED"

pattern Disabling :: TimeToLiveStatus
pattern Disabling = TimeToLiveStatus' "DISABLING"

pattern Enabled :: TimeToLiveStatus
pattern Enabled = TimeToLiveStatus' "ENABLED"

pattern Enabling :: TimeToLiveStatus
pattern Enabling = TimeToLiveStatus' "ENABLING"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
  TimeToLiveStatus'
  #-}
