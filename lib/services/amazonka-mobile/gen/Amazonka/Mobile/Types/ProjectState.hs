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
-- Module      : Amazonka.Mobile.Types.ProjectState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types.ProjectState
  ( ProjectState
      ( ..,
        ProjectState_IMPORTING,
        ProjectState_NORMAL,
        ProjectState_SYNCING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Synchronization state for a project.
newtype ProjectState = ProjectState'
  { fromProjectState ::
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

pattern ProjectState_IMPORTING :: ProjectState
pattern ProjectState_IMPORTING = ProjectState' "IMPORTING"

pattern ProjectState_NORMAL :: ProjectState
pattern ProjectState_NORMAL = ProjectState' "NORMAL"

pattern ProjectState_SYNCING :: ProjectState
pattern ProjectState_SYNCING = ProjectState' "SYNCING"

{-# COMPLETE
  ProjectState_IMPORTING,
  ProjectState_NORMAL,
  ProjectState_SYNCING,
  ProjectState'
  #-}
