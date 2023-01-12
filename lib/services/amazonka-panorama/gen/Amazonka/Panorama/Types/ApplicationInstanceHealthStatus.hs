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
-- Module      : Amazonka.Panorama.Types.ApplicationInstanceHealthStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ApplicationInstanceHealthStatus
  ( ApplicationInstanceHealthStatus
      ( ..,
        ApplicationInstanceHealthStatus_ERROR,
        ApplicationInstanceHealthStatus_NOT_AVAILABLE,
        ApplicationInstanceHealthStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationInstanceHealthStatus = ApplicationInstanceHealthStatus'
  { fromApplicationInstanceHealthStatus ::
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

pattern ApplicationInstanceHealthStatus_ERROR :: ApplicationInstanceHealthStatus
pattern ApplicationInstanceHealthStatus_ERROR = ApplicationInstanceHealthStatus' "ERROR"

pattern ApplicationInstanceHealthStatus_NOT_AVAILABLE :: ApplicationInstanceHealthStatus
pattern ApplicationInstanceHealthStatus_NOT_AVAILABLE = ApplicationInstanceHealthStatus' "NOT_AVAILABLE"

pattern ApplicationInstanceHealthStatus_RUNNING :: ApplicationInstanceHealthStatus
pattern ApplicationInstanceHealthStatus_RUNNING = ApplicationInstanceHealthStatus' "RUNNING"

{-# COMPLETE
  ApplicationInstanceHealthStatus_ERROR,
  ApplicationInstanceHealthStatus_NOT_AVAILABLE,
  ApplicationInstanceHealthStatus_RUNNING,
  ApplicationInstanceHealthStatus'
  #-}
