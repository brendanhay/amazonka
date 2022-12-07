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
-- Module      : Amazonka.GlobalAccelerator.Types.HealthCheckProtocol
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.HealthCheckProtocol
  ( HealthCheckProtocol
      ( ..,
        HealthCheckProtocol_HTTP,
        HealthCheckProtocol_HTTPS,
        HealthCheckProtocol_TCP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HealthCheckProtocol = HealthCheckProtocol'
  { fromHealthCheckProtocol ::
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

pattern HealthCheckProtocol_HTTP :: HealthCheckProtocol
pattern HealthCheckProtocol_HTTP = HealthCheckProtocol' "HTTP"

pattern HealthCheckProtocol_HTTPS :: HealthCheckProtocol
pattern HealthCheckProtocol_HTTPS = HealthCheckProtocol' "HTTPS"

pattern HealthCheckProtocol_TCP :: HealthCheckProtocol
pattern HealthCheckProtocol_TCP = HealthCheckProtocol' "TCP"

{-# COMPLETE
  HealthCheckProtocol_HTTP,
  HealthCheckProtocol_HTTPS,
  HealthCheckProtocol_TCP,
  HealthCheckProtocol'
  #-}
