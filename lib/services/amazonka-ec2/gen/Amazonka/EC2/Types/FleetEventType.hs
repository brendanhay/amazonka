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
-- Module      : Amazonka.EC2.Types.FleetEventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetEventType
  ( FleetEventType
      ( ..,
        FleetEventType_Fleet_change,
        FleetEventType_Instance_change,
        FleetEventType_Service_error
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FleetEventType = FleetEventType'
  { fromFleetEventType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
