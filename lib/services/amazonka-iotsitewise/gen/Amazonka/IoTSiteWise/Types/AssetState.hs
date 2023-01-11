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
-- Module      : Amazonka.IoTSiteWise.Types.AssetState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetState
  ( AssetState
      ( ..,
        AssetState_ACTIVE,
        AssetState_CREATING,
        AssetState_DELETING,
        AssetState_FAILED,
        AssetState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetState = AssetState'
  { fromAssetState ::
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

pattern AssetState_ACTIVE :: AssetState
pattern AssetState_ACTIVE = AssetState' "ACTIVE"

pattern AssetState_CREATING :: AssetState
pattern AssetState_CREATING = AssetState' "CREATING"

pattern AssetState_DELETING :: AssetState
pattern AssetState_DELETING = AssetState' "DELETING"

pattern AssetState_FAILED :: AssetState
pattern AssetState_FAILED = AssetState' "FAILED"

pattern AssetState_UPDATING :: AssetState
pattern AssetState_UPDATING = AssetState' "UPDATING"

{-# COMPLETE
  AssetState_ACTIVE,
  AssetState_CREATING,
  AssetState_DELETING,
  AssetState_FAILED,
  AssetState_UPDATING,
  AssetState'
  #-}
