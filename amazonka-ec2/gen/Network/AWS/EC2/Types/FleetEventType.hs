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
-- Module      : Network.AWS.EC2.Types.FleetEventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetEventType
  ( FleetEventType
      ( ..,
        FleetEventType_Fleet_change,
        FleetEventType_Instance_change,
        FleetEventType_Service_error
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype FleetEventType = FleetEventType'
  { fromFleetEventType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern FleetEventType_Fleet_change :: FleetEventType
pattern FleetEventType_Fleet_change = FleetEventType' "fleet-change"

pattern FleetEventType_Instance_change :: FleetEventType
pattern FleetEventType_Instance_change = FleetEventType' "instance-change"

pattern FleetEventType_Service_error :: FleetEventType
pattern FleetEventType_Service_error = FleetEventType' "service-error"

{-# COMPLETE
  FleetEventType_Fleet_change,
  FleetEventType_Instance_change,
  FleetEventType_Service_error,
  FleetEventType'
  #-}
