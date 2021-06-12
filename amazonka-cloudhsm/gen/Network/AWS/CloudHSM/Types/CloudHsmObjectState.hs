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
-- Module      : Network.AWS.CloudHSM.Types.CloudHsmObjectState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.CloudHsmObjectState
  ( CloudHsmObjectState
      ( ..,
        CloudHsmObjectState_DEGRADED,
        CloudHsmObjectState_READY,
        CloudHsmObjectState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CloudHsmObjectState = CloudHsmObjectState'
  { fromCloudHsmObjectState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern CloudHsmObjectState_DEGRADED :: CloudHsmObjectState
pattern CloudHsmObjectState_DEGRADED = CloudHsmObjectState' "DEGRADED"

pattern CloudHsmObjectState_READY :: CloudHsmObjectState
pattern CloudHsmObjectState_READY = CloudHsmObjectState' "READY"

pattern CloudHsmObjectState_UPDATING :: CloudHsmObjectState
pattern CloudHsmObjectState_UPDATING = CloudHsmObjectState' "UPDATING"

{-# COMPLETE
  CloudHsmObjectState_DEGRADED,
  CloudHsmObjectState_READY,
  CloudHsmObjectState_UPDATING,
  CloudHsmObjectState'
  #-}
