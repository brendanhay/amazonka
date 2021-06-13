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
-- Module      : Network.AWS.GameLift.Types.BuildStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.BuildStatus
  ( BuildStatus
      ( ..,
        BuildStatus_FAILED,
        BuildStatus_INITIALIZED,
        BuildStatus_READY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BuildStatus = BuildStatus'
  { fromBuildStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
