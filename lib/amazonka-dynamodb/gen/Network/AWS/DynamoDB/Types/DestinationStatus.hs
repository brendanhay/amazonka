{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DestinationStatus
  ( DestinationStatus
      ( DestinationStatus',
        DSEnabling,
        DSActive,
        DSDisabling,
        DSDisabled,
        DSEnableFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DestinationStatus = DestinationStatus' Lude.Text
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

pattern DSEnabling :: DestinationStatus
pattern DSEnabling = DestinationStatus' "ENABLING"

pattern DSActive :: DestinationStatus
pattern DSActive = DestinationStatus' "ACTIVE"

pattern DSDisabling :: DestinationStatus
pattern DSDisabling = DestinationStatus' "DISABLING"

pattern DSDisabled :: DestinationStatus
pattern DSDisabled = DestinationStatus' "DISABLED"

pattern DSEnableFailed :: DestinationStatus
pattern DSEnableFailed = DestinationStatus' "ENABLE_FAILED"

{-# COMPLETE
  DSEnabling,
  DSActive,
  DSDisabling,
  DSDisabled,
  DSEnableFailed,
  DestinationStatus'
  #-}
