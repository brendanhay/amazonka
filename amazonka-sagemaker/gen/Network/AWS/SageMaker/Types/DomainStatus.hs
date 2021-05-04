{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype DomainStatus = DomainStatus'
  { fromDomainStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
