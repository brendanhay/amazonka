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
-- Module      : Network.AWS.GreengrassV2.Types.InstalledComponentLifecycleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Types.InstalledComponentLifecycleState
  ( InstalledComponentLifecycleState
      ( ..,
        InstalledComponentLifecycleState_BROKEN,
        InstalledComponentLifecycleState_ERRORED,
        InstalledComponentLifecycleState_FINISHED,
        InstalledComponentLifecycleState_INSTALLED,
        InstalledComponentLifecycleState_NEW,
        InstalledComponentLifecycleState_RUNNING,
        InstalledComponentLifecycleState_STARTING,
        InstalledComponentLifecycleState_STOPPING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstalledComponentLifecycleState = InstalledComponentLifecycleState'
  { fromInstalledComponentLifecycleState ::
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

pattern InstalledComponentLifecycleState_BROKEN :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_BROKEN = InstalledComponentLifecycleState' "BROKEN"

pattern InstalledComponentLifecycleState_ERRORED :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_ERRORED = InstalledComponentLifecycleState' "ERRORED"

pattern InstalledComponentLifecycleState_FINISHED :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_FINISHED = InstalledComponentLifecycleState' "FINISHED"

pattern InstalledComponentLifecycleState_INSTALLED :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_INSTALLED = InstalledComponentLifecycleState' "INSTALLED"

pattern InstalledComponentLifecycleState_NEW :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_NEW = InstalledComponentLifecycleState' "NEW"

pattern InstalledComponentLifecycleState_RUNNING :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_RUNNING = InstalledComponentLifecycleState' "RUNNING"

pattern InstalledComponentLifecycleState_STARTING :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_STARTING = InstalledComponentLifecycleState' "STARTING"

pattern InstalledComponentLifecycleState_STOPPING :: InstalledComponentLifecycleState
pattern InstalledComponentLifecycleState_STOPPING = InstalledComponentLifecycleState' "STOPPING"

{-# COMPLETE
  InstalledComponentLifecycleState_BROKEN,
  InstalledComponentLifecycleState_ERRORED,
  InstalledComponentLifecycleState_FINISHED,
  InstalledComponentLifecycleState_INSTALLED,
  InstalledComponentLifecycleState_NEW,
  InstalledComponentLifecycleState_RUNNING,
  InstalledComponentLifecycleState_STARTING,
  InstalledComponentLifecycleState_STOPPING,
  InstalledComponentLifecycleState'
  #-}
