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
-- Module      : Amazonka.SSMSAP.Types.ApplicationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.ApplicationStatus
  ( ApplicationStatus
      ( ..,
        ApplicationStatus_ACTIVATED,
        ApplicationStatus_DELETING,
        ApplicationStatus_FAILED,
        ApplicationStatus_REGISTERING,
        ApplicationStatus_STARTING,
        ApplicationStatus_STOPPED,
        ApplicationStatus_STOPPING,
        ApplicationStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationStatus = ApplicationStatus'
  { fromApplicationStatus ::
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

pattern ApplicationStatus_ACTIVATED :: ApplicationStatus
pattern ApplicationStatus_ACTIVATED = ApplicationStatus' "ACTIVATED"

pattern ApplicationStatus_DELETING :: ApplicationStatus
pattern ApplicationStatus_DELETING = ApplicationStatus' "DELETING"

pattern ApplicationStatus_FAILED :: ApplicationStatus
pattern ApplicationStatus_FAILED = ApplicationStatus' "FAILED"

pattern ApplicationStatus_REGISTERING :: ApplicationStatus
pattern ApplicationStatus_REGISTERING = ApplicationStatus' "REGISTERING"

pattern ApplicationStatus_STARTING :: ApplicationStatus
pattern ApplicationStatus_STARTING = ApplicationStatus' "STARTING"

pattern ApplicationStatus_STOPPED :: ApplicationStatus
pattern ApplicationStatus_STOPPED = ApplicationStatus' "STOPPED"

pattern ApplicationStatus_STOPPING :: ApplicationStatus
pattern ApplicationStatus_STOPPING = ApplicationStatus' "STOPPING"

pattern ApplicationStatus_UNKNOWN :: ApplicationStatus
pattern ApplicationStatus_UNKNOWN = ApplicationStatus' "UNKNOWN"

{-# COMPLETE
  ApplicationStatus_ACTIVATED,
  ApplicationStatus_DELETING,
  ApplicationStatus_FAILED,
  ApplicationStatus_REGISTERING,
  ApplicationStatus_STARTING,
  ApplicationStatus_STOPPED,
  ApplicationStatus_STOPPING,
  ApplicationStatus_UNKNOWN,
  ApplicationStatus'
  #-}
