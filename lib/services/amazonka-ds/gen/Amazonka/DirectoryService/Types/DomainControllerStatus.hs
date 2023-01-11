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
-- Module      : Amazonka.DirectoryService.Types.DomainControllerStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DomainControllerStatus
  ( DomainControllerStatus
      ( ..,
        DomainControllerStatus_Active,
        DomainControllerStatus_Creating,
        DomainControllerStatus_Deleted,
        DomainControllerStatus_Deleting,
        DomainControllerStatus_Failed,
        DomainControllerStatus_Impaired,
        DomainControllerStatus_Restoring
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainControllerStatus = DomainControllerStatus'
  { fromDomainControllerStatus ::
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

pattern DomainControllerStatus_Active :: DomainControllerStatus
pattern DomainControllerStatus_Active = DomainControllerStatus' "Active"

pattern DomainControllerStatus_Creating :: DomainControllerStatus
pattern DomainControllerStatus_Creating = DomainControllerStatus' "Creating"

pattern DomainControllerStatus_Deleted :: DomainControllerStatus
pattern DomainControllerStatus_Deleted = DomainControllerStatus' "Deleted"

pattern DomainControllerStatus_Deleting :: DomainControllerStatus
pattern DomainControllerStatus_Deleting = DomainControllerStatus' "Deleting"

pattern DomainControllerStatus_Failed :: DomainControllerStatus
pattern DomainControllerStatus_Failed = DomainControllerStatus' "Failed"

pattern DomainControllerStatus_Impaired :: DomainControllerStatus
pattern DomainControllerStatus_Impaired = DomainControllerStatus' "Impaired"

pattern DomainControllerStatus_Restoring :: DomainControllerStatus
pattern DomainControllerStatus_Restoring = DomainControllerStatus' "Restoring"

{-# COMPLETE
  DomainControllerStatus_Active,
  DomainControllerStatus_Creating,
  DomainControllerStatus_Deleted,
  DomainControllerStatus_Deleting,
  DomainControllerStatus_Failed,
  DomainControllerStatus_Impaired,
  DomainControllerStatus_Restoring,
  DomainControllerStatus'
  #-}
