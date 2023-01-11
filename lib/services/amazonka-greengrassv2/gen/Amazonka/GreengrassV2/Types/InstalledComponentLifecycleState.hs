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
-- Module      : Amazonka.GreengrassV2.Types.InstalledComponentLifecycleState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.InstalledComponentLifecycleState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstalledComponentLifecycleState = InstalledComponentLifecycleState'
  { fromInstalledComponentLifecycleState ::
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
