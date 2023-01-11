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
-- Module      : Amazonka.LicenseManager.Types.LicenseCountingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.LicenseCountingType
  ( LicenseCountingType
      ( ..,
        LicenseCountingType_Core,
        LicenseCountingType_Instance,
        LicenseCountingType_Socket,
        LicenseCountingType_VCPU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LicenseCountingType = LicenseCountingType'
  { fromLicenseCountingType ::
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

pattern LicenseCountingType_Core :: LicenseCountingType
pattern LicenseCountingType_Core = LicenseCountingType' "Core"

pattern LicenseCountingType_Instance :: LicenseCountingType
pattern LicenseCountingType_Instance = LicenseCountingType' "Instance"

pattern LicenseCountingType_Socket :: LicenseCountingType
pattern LicenseCountingType_Socket = LicenseCountingType' "Socket"

pattern LicenseCountingType_VCPU :: LicenseCountingType
pattern LicenseCountingType_VCPU = LicenseCountingType' "vCPU"

{-# COMPLETE
  LicenseCountingType_Core,
  LicenseCountingType_Instance,
  LicenseCountingType_Socket,
  LicenseCountingType_VCPU,
  LicenseCountingType'
  #-}
