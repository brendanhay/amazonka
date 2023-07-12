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
-- Module      : Amazonka.Cloud9.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( ..,
        EnvironmentStatus_Connecting,
        EnvironmentStatus_Creating,
        EnvironmentStatus_Deleting,
        EnvironmentStatus_Error,
        EnvironmentStatus_Ready,
        EnvironmentStatus_Stopped,
        EnvironmentStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentStatus = EnvironmentStatus'
  { fromEnvironmentStatus ::
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

pattern EnvironmentStatus_Connecting :: EnvironmentStatus
pattern EnvironmentStatus_Connecting = EnvironmentStatus' "connecting"

pattern EnvironmentStatus_Creating :: EnvironmentStatus
pattern EnvironmentStatus_Creating = EnvironmentStatus' "creating"

pattern EnvironmentStatus_Deleting :: EnvironmentStatus
pattern EnvironmentStatus_Deleting = EnvironmentStatus' "deleting"

pattern EnvironmentStatus_Error :: EnvironmentStatus
pattern EnvironmentStatus_Error = EnvironmentStatus' "error"

pattern EnvironmentStatus_Ready :: EnvironmentStatus
pattern EnvironmentStatus_Ready = EnvironmentStatus' "ready"

pattern EnvironmentStatus_Stopped :: EnvironmentStatus
pattern EnvironmentStatus_Stopped = EnvironmentStatus' "stopped"

pattern EnvironmentStatus_Stopping :: EnvironmentStatus
pattern EnvironmentStatus_Stopping = EnvironmentStatus' "stopping"

{-# COMPLETE
  EnvironmentStatus_Connecting,
  EnvironmentStatus_Creating,
  EnvironmentStatus_Deleting,
  EnvironmentStatus_Error,
  EnvironmentStatus_Ready,
  EnvironmentStatus_Stopped,
  EnvironmentStatus_Stopping,
  EnvironmentStatus'
  #-}
