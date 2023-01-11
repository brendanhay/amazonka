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
-- Module      : Amazonka.Panorama.Types.ApplicationInstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ApplicationInstanceStatus
  ( ApplicationInstanceStatus
      ( ..,
        ApplicationInstanceStatus_DEPLOYMENT_ERROR,
        ApplicationInstanceStatus_DEPLOYMENT_FAILED,
        ApplicationInstanceStatus_DEPLOYMENT_IN_PROGRESS,
        ApplicationInstanceStatus_DEPLOYMENT_PENDING,
        ApplicationInstanceStatus_DEPLOYMENT_REQUESTED,
        ApplicationInstanceStatus_DEPLOYMENT_SUCCEEDED,
        ApplicationInstanceStatus_REMOVAL_FAILED,
        ApplicationInstanceStatus_REMOVAL_IN_PROGRESS,
        ApplicationInstanceStatus_REMOVAL_PENDING,
        ApplicationInstanceStatus_REMOVAL_REQUESTED,
        ApplicationInstanceStatus_REMOVAL_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationInstanceStatus = ApplicationInstanceStatus'
  { fromApplicationInstanceStatus ::
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

pattern ApplicationInstanceStatus_DEPLOYMENT_ERROR :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_ERROR = ApplicationInstanceStatus' "DEPLOYMENT_ERROR"

pattern ApplicationInstanceStatus_DEPLOYMENT_FAILED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_FAILED = ApplicationInstanceStatus' "DEPLOYMENT_FAILED"

pattern ApplicationInstanceStatus_DEPLOYMENT_IN_PROGRESS :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_IN_PROGRESS = ApplicationInstanceStatus' "DEPLOYMENT_IN_PROGRESS"

pattern ApplicationInstanceStatus_DEPLOYMENT_PENDING :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_PENDING = ApplicationInstanceStatus' "DEPLOYMENT_PENDING"

pattern ApplicationInstanceStatus_DEPLOYMENT_REQUESTED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_REQUESTED = ApplicationInstanceStatus' "DEPLOYMENT_REQUESTED"

pattern ApplicationInstanceStatus_DEPLOYMENT_SUCCEEDED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_DEPLOYMENT_SUCCEEDED = ApplicationInstanceStatus' "DEPLOYMENT_SUCCEEDED"

pattern ApplicationInstanceStatus_REMOVAL_FAILED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_REMOVAL_FAILED = ApplicationInstanceStatus' "REMOVAL_FAILED"

pattern ApplicationInstanceStatus_REMOVAL_IN_PROGRESS :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_REMOVAL_IN_PROGRESS = ApplicationInstanceStatus' "REMOVAL_IN_PROGRESS"

pattern ApplicationInstanceStatus_REMOVAL_PENDING :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_REMOVAL_PENDING = ApplicationInstanceStatus' "REMOVAL_PENDING"

pattern ApplicationInstanceStatus_REMOVAL_REQUESTED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_REMOVAL_REQUESTED = ApplicationInstanceStatus' "REMOVAL_REQUESTED"

pattern ApplicationInstanceStatus_REMOVAL_SUCCEEDED :: ApplicationInstanceStatus
pattern ApplicationInstanceStatus_REMOVAL_SUCCEEDED = ApplicationInstanceStatus' "REMOVAL_SUCCEEDED"

{-# COMPLETE
  ApplicationInstanceStatus_DEPLOYMENT_ERROR,
  ApplicationInstanceStatus_DEPLOYMENT_FAILED,
  ApplicationInstanceStatus_DEPLOYMENT_IN_PROGRESS,
  ApplicationInstanceStatus_DEPLOYMENT_PENDING,
  ApplicationInstanceStatus_DEPLOYMENT_REQUESTED,
  ApplicationInstanceStatus_DEPLOYMENT_SUCCEEDED,
  ApplicationInstanceStatus_REMOVAL_FAILED,
  ApplicationInstanceStatus_REMOVAL_IN_PROGRESS,
  ApplicationInstanceStatus_REMOVAL_PENDING,
  ApplicationInstanceStatus_REMOVAL_REQUESTED,
  ApplicationInstanceStatus_REMOVAL_SUCCEEDED,
  ApplicationInstanceStatus'
  #-}
