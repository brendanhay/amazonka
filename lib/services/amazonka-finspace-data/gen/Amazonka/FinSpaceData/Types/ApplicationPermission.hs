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
-- Module      : Amazonka.FinSpaceData.Types.ApplicationPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ApplicationPermission
  ( ApplicationPermission
      ( ..,
        ApplicationPermission_AccessNotebooks,
        ApplicationPermission_CreateDataset,
        ApplicationPermission_GetTemporaryCredentials,
        ApplicationPermission_ManageAttributeSets,
        ApplicationPermission_ManageClusters,
        ApplicationPermission_ManageUsersAndGroups,
        ApplicationPermission_ViewAuditData
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationPermission = ApplicationPermission'
  { fromApplicationPermission ::
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

pattern ApplicationPermission_AccessNotebooks :: ApplicationPermission
pattern ApplicationPermission_AccessNotebooks = ApplicationPermission' "AccessNotebooks"

pattern ApplicationPermission_CreateDataset :: ApplicationPermission
pattern ApplicationPermission_CreateDataset = ApplicationPermission' "CreateDataset"

pattern ApplicationPermission_GetTemporaryCredentials :: ApplicationPermission
pattern ApplicationPermission_GetTemporaryCredentials = ApplicationPermission' "GetTemporaryCredentials"

pattern ApplicationPermission_ManageAttributeSets :: ApplicationPermission
pattern ApplicationPermission_ManageAttributeSets = ApplicationPermission' "ManageAttributeSets"

pattern ApplicationPermission_ManageClusters :: ApplicationPermission
pattern ApplicationPermission_ManageClusters = ApplicationPermission' "ManageClusters"

pattern ApplicationPermission_ManageUsersAndGroups :: ApplicationPermission
pattern ApplicationPermission_ManageUsersAndGroups = ApplicationPermission' "ManageUsersAndGroups"

pattern ApplicationPermission_ViewAuditData :: ApplicationPermission
pattern ApplicationPermission_ViewAuditData = ApplicationPermission' "ViewAuditData"

{-# COMPLETE
  ApplicationPermission_AccessNotebooks,
  ApplicationPermission_CreateDataset,
  ApplicationPermission_GetTemporaryCredentials,
  ApplicationPermission_ManageAttributeSets,
  ApplicationPermission_ManageClusters,
  ApplicationPermission_ManageUsersAndGroups,
  ApplicationPermission_ViewAuditData,
  ApplicationPermission'
  #-}
