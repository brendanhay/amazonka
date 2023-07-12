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
-- Module      : Amazonka.DirectoryService.Types.CertificateState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.CertificateState
  ( CertificateState
      ( ..,
        CertificateState_DeregisterFailed,
        CertificateState_Deregistered,
        CertificateState_Deregistering,
        CertificateState_RegisterFailed,
        CertificateState_Registered,
        CertificateState_Registering
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateState = CertificateState'
  { fromCertificateState ::
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

pattern CertificateState_DeregisterFailed :: CertificateState
pattern CertificateState_DeregisterFailed = CertificateState' "DeregisterFailed"

pattern CertificateState_Deregistered :: CertificateState
pattern CertificateState_Deregistered = CertificateState' "Deregistered"

pattern CertificateState_Deregistering :: CertificateState
pattern CertificateState_Deregistering = CertificateState' "Deregistering"

pattern CertificateState_RegisterFailed :: CertificateState
pattern CertificateState_RegisterFailed = CertificateState' "RegisterFailed"

pattern CertificateState_Registered :: CertificateState
pattern CertificateState_Registered = CertificateState' "Registered"

pattern CertificateState_Registering :: CertificateState
pattern CertificateState_Registering = CertificateState' "Registering"

{-# COMPLETE
  CertificateState_DeregisterFailed,
  CertificateState_Deregistered,
  CertificateState_Deregistering,
  CertificateState_RegisterFailed,
  CertificateState_Registered,
  CertificateState_Registering,
  CertificateState'
  #-}
