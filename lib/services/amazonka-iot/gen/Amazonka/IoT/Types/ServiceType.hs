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
-- Module      : Amazonka.IoT.Types.ServiceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ServiceType
  ( ServiceType
      ( ..,
        ServiceType_CREDENTIAL_PROVIDER,
        ServiceType_DATA,
        ServiceType_JOBS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceType = ServiceType'
  { fromServiceType ::
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

pattern ServiceType_CREDENTIAL_PROVIDER :: ServiceType
pattern ServiceType_CREDENTIAL_PROVIDER = ServiceType' "CREDENTIAL_PROVIDER"

pattern ServiceType_DATA :: ServiceType
pattern ServiceType_DATA = ServiceType' "DATA"

pattern ServiceType_JOBS :: ServiceType
pattern ServiceType_JOBS = ServiceType' "JOBS"

{-# COMPLETE
  ServiceType_CREDENTIAL_PROVIDER,
  ServiceType_DATA,
  ServiceType_JOBS,
  ServiceType'
  #-}
