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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelState
  ( AssetModelState
      ( ..,
        AssetModelState_ACTIVE,
        AssetModelState_CREATING,
        AssetModelState_DELETING,
        AssetModelState_FAILED,
        AssetModelState_PROPAGATING,
        AssetModelState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssetModelState = AssetModelState'
  { fromAssetModelState ::
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

pattern AssetModelState_ACTIVE :: AssetModelState
pattern AssetModelState_ACTIVE = AssetModelState' "ACTIVE"

pattern AssetModelState_CREATING :: AssetModelState
pattern AssetModelState_CREATING = AssetModelState' "CREATING"

pattern AssetModelState_DELETING :: AssetModelState
pattern AssetModelState_DELETING = AssetModelState' "DELETING"

pattern AssetModelState_FAILED :: AssetModelState
pattern AssetModelState_FAILED = AssetModelState' "FAILED"

pattern AssetModelState_PROPAGATING :: AssetModelState
pattern AssetModelState_PROPAGATING = AssetModelState' "PROPAGATING"

pattern AssetModelState_UPDATING :: AssetModelState
pattern AssetModelState_UPDATING = AssetModelState' "UPDATING"

{-# COMPLETE
  AssetModelState_ACTIVE,
  AssetModelState_CREATING,
  AssetModelState_DELETING,
  AssetModelState_FAILED,
  AssetModelState_PROPAGATING,
  AssetModelState_UPDATING,
  AssetModelState'
  #-}
