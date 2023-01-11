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
-- Module      : Amazonka.M2.Types.ApplicationLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.ApplicationLifecycle
  ( ApplicationLifecycle
      ( ..,
        ApplicationLifecycle_Available,
        ApplicationLifecycle_Created,
        ApplicationLifecycle_Creating,
        ApplicationLifecycle_Deleting,
        ApplicationLifecycle_Deleting_From_Environment,
        ApplicationLifecycle_Failed,
        ApplicationLifecycle_Ready,
        ApplicationLifecycle_Running,
        ApplicationLifecycle_Starting,
        ApplicationLifecycle_Stopped,
        ApplicationLifecycle_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationLifecycle = ApplicationLifecycle'
  { fromApplicationLifecycle ::
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

pattern ApplicationLifecycle_Available :: ApplicationLifecycle
pattern ApplicationLifecycle_Available = ApplicationLifecycle' "Available"

pattern ApplicationLifecycle_Created :: ApplicationLifecycle
pattern ApplicationLifecycle_Created = ApplicationLifecycle' "Created"

pattern ApplicationLifecycle_Creating :: ApplicationLifecycle
pattern ApplicationLifecycle_Creating = ApplicationLifecycle' "Creating"

pattern ApplicationLifecycle_Deleting :: ApplicationLifecycle
pattern ApplicationLifecycle_Deleting = ApplicationLifecycle' "Deleting"

pattern ApplicationLifecycle_Deleting_From_Environment :: ApplicationLifecycle
pattern ApplicationLifecycle_Deleting_From_Environment = ApplicationLifecycle' "Deleting From Environment"

pattern ApplicationLifecycle_Failed :: ApplicationLifecycle
pattern ApplicationLifecycle_Failed = ApplicationLifecycle' "Failed"

pattern ApplicationLifecycle_Ready :: ApplicationLifecycle
pattern ApplicationLifecycle_Ready = ApplicationLifecycle' "Ready"

pattern ApplicationLifecycle_Running :: ApplicationLifecycle
pattern ApplicationLifecycle_Running = ApplicationLifecycle' "Running"

pattern ApplicationLifecycle_Starting :: ApplicationLifecycle
pattern ApplicationLifecycle_Starting = ApplicationLifecycle' "Starting"

pattern ApplicationLifecycle_Stopped :: ApplicationLifecycle
pattern ApplicationLifecycle_Stopped = ApplicationLifecycle' "Stopped"

pattern ApplicationLifecycle_Stopping :: ApplicationLifecycle
pattern ApplicationLifecycle_Stopping = ApplicationLifecycle' "Stopping"

{-# COMPLETE
  ApplicationLifecycle_Available,
  ApplicationLifecycle_Created,
  ApplicationLifecycle_Creating,
  ApplicationLifecycle_Deleting,
  ApplicationLifecycle_Deleting_From_Environment,
  ApplicationLifecycle_Failed,
  ApplicationLifecycle_Ready,
  ApplicationLifecycle_Running,
  ApplicationLifecycle_Starting,
  ApplicationLifecycle_Stopped,
  ApplicationLifecycle_Stopping,
  ApplicationLifecycle'
  #-}
