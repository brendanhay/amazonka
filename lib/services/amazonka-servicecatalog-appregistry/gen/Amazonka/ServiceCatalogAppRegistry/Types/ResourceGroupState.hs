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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroupState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroupState
  ( ResourceGroupState
      ( ..,
        ResourceGroupState_CREATE_COMPLETE,
        ResourceGroupState_CREATE_FAILED,
        ResourceGroupState_CREATING,
        ResourceGroupState_UPDATE_COMPLETE,
        ResourceGroupState_UPDATE_FAILED,
        ResourceGroupState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ResourceGroupState = ResourceGroupState'
  { fromResourceGroupState ::
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

pattern ResourceGroupState_CREATE_COMPLETE :: ResourceGroupState
pattern ResourceGroupState_CREATE_COMPLETE = ResourceGroupState' "CREATE_COMPLETE"

pattern ResourceGroupState_CREATE_FAILED :: ResourceGroupState
pattern ResourceGroupState_CREATE_FAILED = ResourceGroupState' "CREATE_FAILED"

pattern ResourceGroupState_CREATING :: ResourceGroupState
pattern ResourceGroupState_CREATING = ResourceGroupState' "CREATING"

pattern ResourceGroupState_UPDATE_COMPLETE :: ResourceGroupState
pattern ResourceGroupState_UPDATE_COMPLETE = ResourceGroupState' "UPDATE_COMPLETE"

pattern ResourceGroupState_UPDATE_FAILED :: ResourceGroupState
pattern ResourceGroupState_UPDATE_FAILED = ResourceGroupState' "UPDATE_FAILED"

pattern ResourceGroupState_UPDATING :: ResourceGroupState
pattern ResourceGroupState_UPDATING = ResourceGroupState' "UPDATING"

{-# COMPLETE
  ResourceGroupState_CREATE_COMPLETE,
  ResourceGroupState_CREATE_FAILED,
  ResourceGroupState_CREATING,
  ResourceGroupState_UPDATE_COMPLETE,
  ResourceGroupState_UPDATE_FAILED,
  ResourceGroupState_UPDATING,
  ResourceGroupState'
  #-}
