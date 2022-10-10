{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.EventCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EventCode
  ( EventCode
      ( ..,
        EventCode_Instance_reboot,
        EventCode_Instance_retirement,
        EventCode_Instance_stop,
        EventCode_System_maintenance,
        EventCode_System_reboot
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype EventCode = EventCode'
  { fromEventCode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern EventCode_Instance_reboot :: EventCode
pattern EventCode_Instance_reboot = EventCode' "instance-reboot"

pattern EventCode_Instance_retirement :: EventCode
pattern EventCode_Instance_retirement = EventCode' "instance-retirement"

pattern EventCode_Instance_stop :: EventCode
pattern EventCode_Instance_stop = EventCode' "instance-stop"

pattern EventCode_System_maintenance :: EventCode
pattern EventCode_System_maintenance = EventCode' "system-maintenance"

pattern EventCode_System_reboot :: EventCode
pattern EventCode_System_reboot = EventCode' "system-reboot"

{-# COMPLETE
  EventCode_Instance_reboot,
  EventCode_Instance_retirement,
  EventCode_Instance_stop,
  EventCode_System_maintenance,
  EventCode_System_reboot,
  EventCode'
  #-}
