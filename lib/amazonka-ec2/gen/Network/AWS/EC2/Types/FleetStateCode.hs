{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetStateCode
  ( FleetStateCode
      ( FleetStateCode',
        FSCActive,
        FSCDeleted,
        FSCDeletedRunning,
        FSCDeletedTerminating,
        FSCFailed,
        FSCModifying,
        FSCSubmitted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FleetStateCode = FleetStateCode' Lude.Text
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

pattern FSCActive :: FleetStateCode
pattern FSCActive = FleetStateCode' "active"

pattern FSCDeleted :: FleetStateCode
pattern FSCDeleted = FleetStateCode' "deleted"

pattern FSCDeletedRunning :: FleetStateCode
pattern FSCDeletedRunning = FleetStateCode' "deleted_running"

pattern FSCDeletedTerminating :: FleetStateCode
pattern FSCDeletedTerminating = FleetStateCode' "deleted_terminating"

pattern FSCFailed :: FleetStateCode
pattern FSCFailed = FleetStateCode' "failed"

pattern FSCModifying :: FleetStateCode
pattern FSCModifying = FleetStateCode' "modifying"

pattern FSCSubmitted :: FleetStateCode
pattern FSCSubmitted = FleetStateCode' "submitted"

{-# COMPLETE
  FSCActive,
  FSCDeleted,
  FSCDeletedRunning,
  FSCDeletedTerminating,
  FSCFailed,
  FSCModifying,
  FSCSubmitted,
  FleetStateCode'
  #-}
