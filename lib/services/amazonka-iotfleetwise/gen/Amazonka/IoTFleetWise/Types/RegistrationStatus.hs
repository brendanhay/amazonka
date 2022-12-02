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
-- Module      : Amazonka.IoTFleetWise.Types.RegistrationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.RegistrationStatus
  ( RegistrationStatus
      ( ..,
        RegistrationStatus_REGISTRATION_FAILURE,
        RegistrationStatus_REGISTRATION_PENDING,
        RegistrationStatus_REGISTRATION_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RegistrationStatus = RegistrationStatus'
  { fromRegistrationStatus ::
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

pattern RegistrationStatus_REGISTRATION_FAILURE :: RegistrationStatus
pattern RegistrationStatus_REGISTRATION_FAILURE = RegistrationStatus' "REGISTRATION_FAILURE"

pattern RegistrationStatus_REGISTRATION_PENDING :: RegistrationStatus
pattern RegistrationStatus_REGISTRATION_PENDING = RegistrationStatus' "REGISTRATION_PENDING"

pattern RegistrationStatus_REGISTRATION_SUCCESS :: RegistrationStatus
pattern RegistrationStatus_REGISTRATION_SUCCESS = RegistrationStatus' "REGISTRATION_SUCCESS"

{-# COMPLETE
  RegistrationStatus_REGISTRATION_FAILURE,
  RegistrationStatus_REGISTRATION_PENDING,
  RegistrationStatus_REGISTRATION_SUCCESS,
  RegistrationStatus'
  #-}
