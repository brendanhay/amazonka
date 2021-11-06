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
-- Module      : Amazonka.MediaConnect.Types.Status
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Status
  ( Status
      ( ..,
        Status_ACTIVE,
        Status_DELETING,
        Status_ERROR,
        Status_STANDBY,
        Status_STARTING,
        Status_STOPPING,
        Status_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Core.Text}
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

pattern Status_ACTIVE :: Status
pattern Status_ACTIVE = Status' "ACTIVE"

pattern Status_DELETING :: Status
pattern Status_DELETING = Status' "DELETING"

pattern Status_ERROR :: Status
pattern Status_ERROR = Status' "ERROR"

pattern Status_STANDBY :: Status
pattern Status_STANDBY = Status' "STANDBY"

pattern Status_STARTING :: Status
pattern Status_STARTING = Status' "STARTING"

pattern Status_STOPPING :: Status
pattern Status_STOPPING = Status' "STOPPING"

pattern Status_UPDATING :: Status
pattern Status_UPDATING = Status' "UPDATING"

{-# COMPLETE
  Status_ACTIVE,
  Status_DELETING,
  Status_ERROR,
  Status_STANDBY,
  Status_STARTING,
  Status_STOPPING,
  Status_UPDATING,
  Status'
  #-}
