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
-- Module      : Amazonka.MediaConnect.Types.InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.InstanceState
  ( InstanceState
      ( ..,
        InstanceState_ACTIVE,
        InstanceState_DEREGISTERED,
        InstanceState_DEREGISTERING,
        InstanceState_DEREGISTRATION_ERROR,
        InstanceState_REGISTERING,
        InstanceState_REGISTRATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceState = InstanceState'
  { fromInstanceState ::
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

pattern InstanceState_ACTIVE :: InstanceState
pattern InstanceState_ACTIVE = InstanceState' "ACTIVE"

pattern InstanceState_DEREGISTERED :: InstanceState
pattern InstanceState_DEREGISTERED = InstanceState' "DEREGISTERED"

pattern InstanceState_DEREGISTERING :: InstanceState
pattern InstanceState_DEREGISTERING = InstanceState' "DEREGISTERING"

pattern InstanceState_DEREGISTRATION_ERROR :: InstanceState
pattern InstanceState_DEREGISTRATION_ERROR = InstanceState' "DEREGISTRATION_ERROR"

pattern InstanceState_REGISTERING :: InstanceState
pattern InstanceState_REGISTERING = InstanceState' "REGISTERING"

pattern InstanceState_REGISTRATION_ERROR :: InstanceState
pattern InstanceState_REGISTRATION_ERROR = InstanceState' "REGISTRATION_ERROR"

{-# COMPLETE
  InstanceState_ACTIVE,
  InstanceState_DEREGISTERED,
  InstanceState_DEREGISTERING,
  InstanceState_DEREGISTRATION_ERROR,
  InstanceState_REGISTERING,
  InstanceState_REGISTRATION_ERROR,
  InstanceState'
  #-}
