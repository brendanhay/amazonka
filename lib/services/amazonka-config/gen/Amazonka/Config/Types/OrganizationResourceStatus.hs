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
-- Module      : Amazonka.Config.Types.OrganizationResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationResourceStatus
  ( OrganizationResourceStatus
      ( ..,
        OrganizationResourceStatus_CREATE_FAILED,
        OrganizationResourceStatus_CREATE_IN_PROGRESS,
        OrganizationResourceStatus_CREATE_SUCCESSFUL,
        OrganizationResourceStatus_DELETE_FAILED,
        OrganizationResourceStatus_DELETE_IN_PROGRESS,
        OrganizationResourceStatus_DELETE_SUCCESSFUL,
        OrganizationResourceStatus_UPDATE_FAILED,
        OrganizationResourceStatus_UPDATE_IN_PROGRESS,
        OrganizationResourceStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrganizationResourceStatus = OrganizationResourceStatus'
  { fromOrganizationResourceStatus ::
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

pattern OrganizationResourceStatus_CREATE_FAILED :: OrganizationResourceStatus
pattern OrganizationResourceStatus_CREATE_FAILED = OrganizationResourceStatus' "CREATE_FAILED"

pattern OrganizationResourceStatus_CREATE_IN_PROGRESS :: OrganizationResourceStatus
pattern OrganizationResourceStatus_CREATE_IN_PROGRESS = OrganizationResourceStatus' "CREATE_IN_PROGRESS"

pattern OrganizationResourceStatus_CREATE_SUCCESSFUL :: OrganizationResourceStatus
pattern OrganizationResourceStatus_CREATE_SUCCESSFUL = OrganizationResourceStatus' "CREATE_SUCCESSFUL"

pattern OrganizationResourceStatus_DELETE_FAILED :: OrganizationResourceStatus
pattern OrganizationResourceStatus_DELETE_FAILED = OrganizationResourceStatus' "DELETE_FAILED"

pattern OrganizationResourceStatus_DELETE_IN_PROGRESS :: OrganizationResourceStatus
pattern OrganizationResourceStatus_DELETE_IN_PROGRESS = OrganizationResourceStatus' "DELETE_IN_PROGRESS"

pattern OrganizationResourceStatus_DELETE_SUCCESSFUL :: OrganizationResourceStatus
pattern OrganizationResourceStatus_DELETE_SUCCESSFUL = OrganizationResourceStatus' "DELETE_SUCCESSFUL"

pattern OrganizationResourceStatus_UPDATE_FAILED :: OrganizationResourceStatus
pattern OrganizationResourceStatus_UPDATE_FAILED = OrganizationResourceStatus' "UPDATE_FAILED"

pattern OrganizationResourceStatus_UPDATE_IN_PROGRESS :: OrganizationResourceStatus
pattern OrganizationResourceStatus_UPDATE_IN_PROGRESS = OrganizationResourceStatus' "UPDATE_IN_PROGRESS"

pattern OrganizationResourceStatus_UPDATE_SUCCESSFUL :: OrganizationResourceStatus
pattern OrganizationResourceStatus_UPDATE_SUCCESSFUL = OrganizationResourceStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  OrganizationResourceStatus_CREATE_FAILED,
  OrganizationResourceStatus_CREATE_IN_PROGRESS,
  OrganizationResourceStatus_CREATE_SUCCESSFUL,
  OrganizationResourceStatus_DELETE_FAILED,
  OrganizationResourceStatus_DELETE_IN_PROGRESS,
  OrganizationResourceStatus_DELETE_SUCCESSFUL,
  OrganizationResourceStatus_UPDATE_FAILED,
  OrganizationResourceStatus_UPDATE_IN_PROGRESS,
  OrganizationResourceStatus_UPDATE_SUCCESSFUL,
  OrganizationResourceStatus'
  #-}
