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
-- Module      : Network.AWS.IoTSiteWise.Types.AssetState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.AssetState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AssetState = AssetState'
  { fromAssetState ::
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
