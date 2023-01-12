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
-- Module      : Amazonka.Chime.Types.RegistrationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.RegistrationStatus
  ( RegistrationStatus
      ( ..,
        RegistrationStatus_Registered,
        RegistrationStatus_Suspended,
        RegistrationStatus_Unregistered
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

pattern RegistrationStatus_Registered :: RegistrationStatus
pattern RegistrationStatus_Registered = RegistrationStatus' "Registered"

pattern RegistrationStatus_Suspended :: RegistrationStatus
pattern RegistrationStatus_Suspended = RegistrationStatus' "Suspended"

pattern RegistrationStatus_Unregistered :: RegistrationStatus
pattern RegistrationStatus_Unregistered = RegistrationStatus' "Unregistered"

{-# COMPLETE
  RegistrationStatus_Registered,
  RegistrationStatus_Suspended,
  RegistrationStatus_Unregistered,
  RegistrationStatus'
  #-}
