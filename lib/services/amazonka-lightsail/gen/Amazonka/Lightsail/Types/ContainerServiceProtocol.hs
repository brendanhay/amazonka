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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceProtocol
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceProtocol
  ( ContainerServiceProtocol
      ( ..,
        ContainerServiceProtocol_HTTP,
        ContainerServiceProtocol_HTTPS,
        ContainerServiceProtocol_TCP,
        ContainerServiceProtocol_UDP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerServiceProtocol = ContainerServiceProtocol'
  { fromContainerServiceProtocol ::
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

pattern ContainerServiceProtocol_HTTP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_HTTP = ContainerServiceProtocol' "HTTP"

pattern ContainerServiceProtocol_HTTPS :: ContainerServiceProtocol
pattern ContainerServiceProtocol_HTTPS = ContainerServiceProtocol' "HTTPS"

pattern ContainerServiceProtocol_TCP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_TCP = ContainerServiceProtocol' "TCP"

pattern ContainerServiceProtocol_UDP :: ContainerServiceProtocol
pattern ContainerServiceProtocol_UDP = ContainerServiceProtocol' "UDP"

{-# COMPLETE
  ContainerServiceProtocol_HTTP,
  ContainerServiceProtocol_HTTPS,
  ContainerServiceProtocol_TCP,
  ContainerServiceProtocol_UDP,
  ContainerServiceProtocol'
  #-}
