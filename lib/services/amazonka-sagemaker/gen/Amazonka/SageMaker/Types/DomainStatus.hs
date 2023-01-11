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
-- Module      : Amazonka.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DomainStatus
  ( DomainStatus
      ( ..,
        DomainStatus_Delete_Failed,
        DomainStatus_Deleting,
        DomainStatus_Failed,
        DomainStatus_InService,
        DomainStatus_Pending,
        DomainStatus_Update_Failed,
        DomainStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainStatus = DomainStatus'
  { fromDomainStatus ::
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

pattern DomainStatus_Delete_Failed :: DomainStatus
pattern DomainStatus_Delete_Failed = DomainStatus' "Delete_Failed"

pattern DomainStatus_Deleting :: DomainStatus
pattern DomainStatus_Deleting = DomainStatus' "Deleting"

pattern DomainStatus_Failed :: DomainStatus
pattern DomainStatus_Failed = DomainStatus' "Failed"

pattern DomainStatus_InService :: DomainStatus
pattern DomainStatus_InService = DomainStatus' "InService"

pattern DomainStatus_Pending :: DomainStatus
pattern DomainStatus_Pending = DomainStatus' "Pending"

pattern DomainStatus_Update_Failed :: DomainStatus
pattern DomainStatus_Update_Failed = DomainStatus' "Update_Failed"

pattern DomainStatus_Updating :: DomainStatus
pattern DomainStatus_Updating = DomainStatus' "Updating"

{-# COMPLETE
  DomainStatus_Delete_Failed,
  DomainStatus_Deleting,
  DomainStatus_Failed,
  DomainStatus_InService,
  DomainStatus_Pending,
  DomainStatus_Update_Failed,
  DomainStatus_Updating,
  DomainStatus'
  #-}
