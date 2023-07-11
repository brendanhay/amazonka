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
-- Module      : Amazonka.DeviceFarm.Types.InstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.InstanceStatus
  ( InstanceStatus
      ( ..,
        InstanceStatus_AVAILABLE,
        InstanceStatus_IN_USE,
        InstanceStatus_NOT_AVAILABLE,
        InstanceStatus_PREPARING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatus_AVAILABLE :: InstanceStatus
pattern InstanceStatus_AVAILABLE = InstanceStatus' "AVAILABLE"

pattern InstanceStatus_IN_USE :: InstanceStatus
pattern InstanceStatus_IN_USE = InstanceStatus' "IN_USE"

pattern InstanceStatus_NOT_AVAILABLE :: InstanceStatus
pattern InstanceStatus_NOT_AVAILABLE = InstanceStatus' "NOT_AVAILABLE"

pattern InstanceStatus_PREPARING :: InstanceStatus
pattern InstanceStatus_PREPARING = InstanceStatus' "PREPARING"

{-# COMPLETE
  InstanceStatus_AVAILABLE,
  InstanceStatus_IN_USE,
  InstanceStatus_NOT_AVAILABLE,
  InstanceStatus_PREPARING,
  InstanceStatus'
  #-}
