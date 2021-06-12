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
-- Module      : Network.AWS.DirectoryService.Types.DomainControllerStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainControllerStatus
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

import qualified Network.AWS.Core as Core

newtype DomainControllerStatus = DomainControllerStatus'
  { fromDomainControllerStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
