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
-- Module      : Amazonka.NetworkManager.Types.LinkState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.LinkState
  ( LinkState
      ( ..,
        LinkState_AVAILABLE,
        LinkState_DELETING,
        LinkState_PENDING,
        LinkState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LinkState = LinkState'
  { fromLinkState ::
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

pattern LinkState_AVAILABLE :: LinkState
pattern LinkState_AVAILABLE = LinkState' "AVAILABLE"

pattern LinkState_DELETING :: LinkState
pattern LinkState_DELETING = LinkState' "DELETING"

pattern LinkState_PENDING :: LinkState
pattern LinkState_PENDING = LinkState' "PENDING"

pattern LinkState_UPDATING :: LinkState
pattern LinkState_UPDATING = LinkState' "UPDATING"

{-# COMPLETE
  LinkState_AVAILABLE,
  LinkState_DELETING,
  LinkState_PENDING,
  LinkState_UPDATING,
  LinkState'
  #-}
