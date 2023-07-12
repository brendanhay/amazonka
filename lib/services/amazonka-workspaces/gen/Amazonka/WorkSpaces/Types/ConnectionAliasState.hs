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
-- Module      : Amazonka.WorkSpaces.Types.ConnectionAliasState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ConnectionAliasState
  ( ConnectionAliasState
      ( ..,
        ConnectionAliasState_CREATED,
        ConnectionAliasState_CREATING,
        ConnectionAliasState_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectionAliasState = ConnectionAliasState'
  { fromConnectionAliasState ::
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

pattern ConnectionAliasState_CREATED :: ConnectionAliasState
pattern ConnectionAliasState_CREATED = ConnectionAliasState' "CREATED"

pattern ConnectionAliasState_CREATING :: ConnectionAliasState
pattern ConnectionAliasState_CREATING = ConnectionAliasState' "CREATING"

pattern ConnectionAliasState_DELETING :: ConnectionAliasState
pattern ConnectionAliasState_DELETING = ConnectionAliasState' "DELETING"

{-# COMPLETE
  ConnectionAliasState_CREATED,
  ConnectionAliasState_CREATING,
  ConnectionAliasState_DELETING,
  ConnectionAliasState'
  #-}
