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
-- Module      : Amazonka.ResourceExplorer2.Types.IndexState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.IndexState
  ( IndexState
      ( ..,
        IndexState_ACTIVE,
        IndexState_CREATING,
        IndexState_DELETED,
        IndexState_DELETING,
        IndexState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IndexState = IndexState'
  { fromIndexState ::
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

pattern IndexState_ACTIVE :: IndexState
pattern IndexState_ACTIVE = IndexState' "ACTIVE"

pattern IndexState_CREATING :: IndexState
pattern IndexState_CREATING = IndexState' "CREATING"

pattern IndexState_DELETED :: IndexState
pattern IndexState_DELETED = IndexState' "DELETED"

pattern IndexState_DELETING :: IndexState
pattern IndexState_DELETING = IndexState' "DELETING"

pattern IndexState_UPDATING :: IndexState
pattern IndexState_UPDATING = IndexState' "UPDATING"

{-# COMPLETE
  IndexState_ACTIVE,
  IndexState_CREATING,
  IndexState_DELETED,
  IndexState_DELETING,
  IndexState_UPDATING,
  IndexState'
  #-}
