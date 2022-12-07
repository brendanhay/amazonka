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
-- Module      : Amazonka.CloudHSM.Types.CloudHsmObjectState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSM.Types.CloudHsmObjectState
  ( CloudHsmObjectState
      ( ..,
        CloudHsmObjectState_DEGRADED,
        CloudHsmObjectState_READY,
        CloudHsmObjectState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CloudHsmObjectState = CloudHsmObjectState'
  { fromCloudHsmObjectState ::
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
