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
-- Module      : Amazonka.GreengrassV2.Types.CloudComponentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.CloudComponentState
  ( CloudComponentState
      ( ..,
        CloudComponentState_DEPLOYABLE,
        CloudComponentState_DEPRECATED,
        CloudComponentState_FAILED,
        CloudComponentState_INITIATED,
        CloudComponentState_REQUESTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CloudComponentState = CloudComponentState'
  { fromCloudComponentState ::
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

pattern CloudComponentState_DEPLOYABLE :: CloudComponentState
pattern CloudComponentState_DEPLOYABLE = CloudComponentState' "DEPLOYABLE"

pattern CloudComponentState_DEPRECATED :: CloudComponentState
pattern CloudComponentState_DEPRECATED = CloudComponentState' "DEPRECATED"

pattern CloudComponentState_FAILED :: CloudComponentState
pattern CloudComponentState_FAILED = CloudComponentState' "FAILED"

pattern CloudComponentState_INITIATED :: CloudComponentState
pattern CloudComponentState_INITIATED = CloudComponentState' "INITIATED"

pattern CloudComponentState_REQUESTED :: CloudComponentState
pattern CloudComponentState_REQUESTED = CloudComponentState' "REQUESTED"

{-# COMPLETE
  CloudComponentState_DEPLOYABLE,
  CloudComponentState_DEPRECATED,
  CloudComponentState_FAILED,
  CloudComponentState_INITIATED,
  CloudComponentState_REQUESTED,
  CloudComponentState'
  #-}
