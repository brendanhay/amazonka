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
-- Module      : Amazonka.SageMaker.Types.HubStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubStatus
  ( HubStatus
      ( ..,
        HubStatus_CreateFailed,
        HubStatus_Creating,
        HubStatus_DeleteFailed,
        HubStatus_Deleting,
        HubStatus_InService,
        HubStatus_UpdateFailed,
        HubStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HubStatus = HubStatus'
  { fromHubStatus ::
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

pattern HubStatus_CreateFailed :: HubStatus
pattern HubStatus_CreateFailed = HubStatus' "CreateFailed"

pattern HubStatus_Creating :: HubStatus
pattern HubStatus_Creating = HubStatus' "Creating"

pattern HubStatus_DeleteFailed :: HubStatus
pattern HubStatus_DeleteFailed = HubStatus' "DeleteFailed"

pattern HubStatus_Deleting :: HubStatus
pattern HubStatus_Deleting = HubStatus' "Deleting"

pattern HubStatus_InService :: HubStatus
pattern HubStatus_InService = HubStatus' "InService"

pattern HubStatus_UpdateFailed :: HubStatus
pattern HubStatus_UpdateFailed = HubStatus' "UpdateFailed"

pattern HubStatus_Updating :: HubStatus
pattern HubStatus_Updating = HubStatus' "Updating"

{-# COMPLETE
  HubStatus_CreateFailed,
  HubStatus_Creating,
  HubStatus_DeleteFailed,
  HubStatus_Deleting,
  HubStatus_InService,
  HubStatus_UpdateFailed,
  HubStatus_Updating,
  HubStatus'
  #-}
