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
-- Module      : Amazonka.MGN.Types.ApplicationHealthStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ApplicationHealthStatus
  ( ApplicationHealthStatus
      ( ..,
        ApplicationHealthStatus_ERROR,
        ApplicationHealthStatus_HEALTHY,
        ApplicationHealthStatus_LAGGING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationHealthStatus = ApplicationHealthStatus'
  { fromApplicationHealthStatus ::
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

pattern ApplicationHealthStatus_ERROR :: ApplicationHealthStatus
pattern ApplicationHealthStatus_ERROR = ApplicationHealthStatus' "ERROR"

pattern ApplicationHealthStatus_HEALTHY :: ApplicationHealthStatus
pattern ApplicationHealthStatus_HEALTHY = ApplicationHealthStatus' "HEALTHY"

pattern ApplicationHealthStatus_LAGGING :: ApplicationHealthStatus
pattern ApplicationHealthStatus_LAGGING = ApplicationHealthStatus' "LAGGING"

{-# COMPLETE
  ApplicationHealthStatus_ERROR,
  ApplicationHealthStatus_HEALTHY,
  ApplicationHealthStatus_LAGGING,
  ApplicationHealthStatus'
  #-}
