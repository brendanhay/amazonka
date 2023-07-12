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
-- Module      : Amazonka.EC2.Types.FleetStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetStateCode
  ( FleetStateCode
      ( ..,
        FleetStateCode_Active,
        FleetStateCode_Deleted,
        FleetStateCode_Deleted_running,
        FleetStateCode_Deleted_terminating,
        FleetStateCode_Failed,
        FleetStateCode_Modifying,
        FleetStateCode_Submitted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FleetStateCode = FleetStateCode'
  { fromFleetStateCode ::
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

pattern FleetStateCode_Active :: FleetStateCode
pattern FleetStateCode_Active = FleetStateCode' "active"

pattern FleetStateCode_Deleted :: FleetStateCode
pattern FleetStateCode_Deleted = FleetStateCode' "deleted"

pattern FleetStateCode_Deleted_running :: FleetStateCode
pattern FleetStateCode_Deleted_running = FleetStateCode' "deleted_running"

pattern FleetStateCode_Deleted_terminating :: FleetStateCode
pattern FleetStateCode_Deleted_terminating = FleetStateCode' "deleted_terminating"

pattern FleetStateCode_Failed :: FleetStateCode
pattern FleetStateCode_Failed = FleetStateCode' "failed"

pattern FleetStateCode_Modifying :: FleetStateCode
pattern FleetStateCode_Modifying = FleetStateCode' "modifying"

pattern FleetStateCode_Submitted :: FleetStateCode
pattern FleetStateCode_Submitted = FleetStateCode' "submitted"

{-# COMPLETE
  FleetStateCode_Active,
  FleetStateCode_Deleted,
  FleetStateCode_Deleted_running,
  FleetStateCode_Deleted_terminating,
  FleetStateCode_Failed,
  FleetStateCode_Modifying,
  FleetStateCode_Submitted,
  FleetStateCode'
  #-}
