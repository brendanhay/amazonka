{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MonitoringState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MonitoringState
  ( MonitoringState
      ( MonitoringState',
        MSDisabled,
        MSDisabling,
        MSEnabled,
        MSPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MonitoringState = MonitoringState' Lude.Text
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

pattern MSDisabled :: MonitoringState
pattern MSDisabled = MonitoringState' "disabled"

pattern MSDisabling :: MonitoringState
pattern MSDisabling = MonitoringState' "disabling"

pattern MSEnabled :: MonitoringState
pattern MSEnabled = MonitoringState' "enabled"

pattern MSPending :: MonitoringState
pattern MSPending = MonitoringState' "pending"

{-# COMPLETE
  MSDisabled,
  MSDisabling,
  MSEnabled,
  MSPending,
  MonitoringState'
  #-}
