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
-- Module      : Amazonka.TNB.Types.NsState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.NsState
  ( NsState
      ( ..,
        NsState_DELETED,
        NsState_IMPAIRED,
        NsState_INSTANTIATED,
        NsState_INSTANTIATE_IN_PROGRESS,
        NsState_NOT_INSTANTIATED,
        NsState_STOPPED,
        NsState_TERMINATE_IN_PROGRESS,
        NsState_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NsState = NsState' {fromNsState :: Data.Text}
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

pattern NsState_DELETED :: NsState
pattern NsState_DELETED = NsState' "DELETED"

pattern NsState_IMPAIRED :: NsState
pattern NsState_IMPAIRED = NsState' "IMPAIRED"

pattern NsState_INSTANTIATED :: NsState
pattern NsState_INSTANTIATED = NsState' "INSTANTIATED"

pattern NsState_INSTANTIATE_IN_PROGRESS :: NsState
pattern NsState_INSTANTIATE_IN_PROGRESS = NsState' "INSTANTIATE_IN_PROGRESS"

pattern NsState_NOT_INSTANTIATED :: NsState
pattern NsState_NOT_INSTANTIATED = NsState' "NOT_INSTANTIATED"

pattern NsState_STOPPED :: NsState
pattern NsState_STOPPED = NsState' "STOPPED"

pattern NsState_TERMINATE_IN_PROGRESS :: NsState
pattern NsState_TERMINATE_IN_PROGRESS = NsState' "TERMINATE_IN_PROGRESS"

pattern NsState_UPDATE_IN_PROGRESS :: NsState
pattern NsState_UPDATE_IN_PROGRESS = NsState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  NsState_DELETED,
  NsState_IMPAIRED,
  NsState_INSTANTIATED,
  NsState_INSTANTIATE_IN_PROGRESS,
  NsState_NOT_INSTANTIATED,
  NsState_STOPPED,
  NsState_TERMINATE_IN_PROGRESS,
  NsState_UPDATE_IN_PROGRESS,
  NsState'
  #-}
