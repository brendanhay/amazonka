{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventCode
  ( EventCode
      ( EventCode',
        InstanceReboot,
        InstanceRetirement,
        InstanceStop,
        SystemMaintenance,
        SystemReboot
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventCode = EventCode' Lude.Text
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

pattern InstanceReboot :: EventCode
pattern InstanceReboot = EventCode' "instance-reboot"

pattern InstanceRetirement :: EventCode
pattern InstanceRetirement = EventCode' "instance-retirement"

pattern InstanceStop :: EventCode
pattern InstanceStop = EventCode' "instance-stop"

pattern SystemMaintenance :: EventCode
pattern SystemMaintenance = EventCode' "system-maintenance"

pattern SystemReboot :: EventCode
pattern SystemReboot = EventCode' "system-reboot"

{-# COMPLETE
  InstanceReboot,
  InstanceRetirement,
  InstanceStop,
  SystemMaintenance,
  SystemReboot,
  EventCode'
  #-}
