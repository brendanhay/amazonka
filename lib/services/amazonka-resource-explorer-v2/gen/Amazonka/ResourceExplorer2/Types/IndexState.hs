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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IndexState = IndexState'
  { fromIndexState ::
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
