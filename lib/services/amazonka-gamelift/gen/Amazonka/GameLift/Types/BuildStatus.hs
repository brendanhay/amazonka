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
-- Module      : Amazonka.GameLift.Types.BuildStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.BuildStatus
  ( BuildStatus
      ( ..,
        BuildStatus_FAILED,
        BuildStatus_INITIALIZED,
        BuildStatus_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BuildStatus = BuildStatus'
  { fromBuildStatus ::
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

pattern BuildStatus_FAILED :: BuildStatus
pattern BuildStatus_FAILED = BuildStatus' "FAILED"

pattern BuildStatus_INITIALIZED :: BuildStatus
pattern BuildStatus_INITIALIZED = BuildStatus' "INITIALIZED"

pattern BuildStatus_READY :: BuildStatus
pattern BuildStatus_READY = BuildStatus' "READY"

{-# COMPLETE
  BuildStatus_FAILED,
  BuildStatus_INITIALIZED,
  BuildStatus_READY,
  BuildStatus'
  #-}
