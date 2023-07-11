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
-- Module      : Amazonka.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationResourceDetailedStatus
  ( OrganizationResourceDetailedStatus
      ( ..,
        OrganizationResourceDetailedStatus_CREATE_FAILED,
        OrganizationResourceDetailedStatus_CREATE_IN_PROGRESS,
        OrganizationResourceDetailedStatus_CREATE_SUCCESSFUL,
        OrganizationResourceDetailedStatus_DELETE_FAILED,
        OrganizationResourceDetailedStatus_DELETE_IN_PROGRESS,
        OrganizationResourceDetailedStatus_DELETE_SUCCESSFUL,
        OrganizationResourceDetailedStatus_UPDATE_FAILED,
        OrganizationResourceDetailedStatus_UPDATE_IN_PROGRESS,
        OrganizationResourceDetailedStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrganizationResourceDetailedStatus = OrganizationResourceDetailedStatus'
  { fromOrganizationResourceDetailedStatus ::
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

pattern OrganizationResourceDetailedStatus_CREATE_FAILED :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_CREATE_FAILED = OrganizationResourceDetailedStatus' "CREATE_FAILED"

pattern OrganizationResourceDetailedStatus_CREATE_IN_PROGRESS :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_CREATE_IN_PROGRESS = OrganizationResourceDetailedStatus' "CREATE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatus_CREATE_SUCCESSFUL :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_CREATE_SUCCESSFUL = OrganizationResourceDetailedStatus' "CREATE_SUCCESSFUL"

pattern OrganizationResourceDetailedStatus_DELETE_FAILED :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_DELETE_FAILED = OrganizationResourceDetailedStatus' "DELETE_FAILED"

pattern OrganizationResourceDetailedStatus_DELETE_IN_PROGRESS :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_DELETE_IN_PROGRESS = OrganizationResourceDetailedStatus' "DELETE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatus_DELETE_SUCCESSFUL :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_DELETE_SUCCESSFUL = OrganizationResourceDetailedStatus' "DELETE_SUCCESSFUL"

pattern OrganizationResourceDetailedStatus_UPDATE_FAILED :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_UPDATE_FAILED = OrganizationResourceDetailedStatus' "UPDATE_FAILED"

pattern OrganizationResourceDetailedStatus_UPDATE_IN_PROGRESS :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_UPDATE_IN_PROGRESS = OrganizationResourceDetailedStatus' "UPDATE_IN_PROGRESS"

pattern OrganizationResourceDetailedStatus_UPDATE_SUCCESSFUL :: OrganizationResourceDetailedStatus
pattern OrganizationResourceDetailedStatus_UPDATE_SUCCESSFUL = OrganizationResourceDetailedStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  OrganizationResourceDetailedStatus_CREATE_FAILED,
  OrganizationResourceDetailedStatus_CREATE_IN_PROGRESS,
  OrganizationResourceDetailedStatus_CREATE_SUCCESSFUL,
  OrganizationResourceDetailedStatus_DELETE_FAILED,
  OrganizationResourceDetailedStatus_DELETE_IN_PROGRESS,
  OrganizationResourceDetailedStatus_DELETE_SUCCESSFUL,
  OrganizationResourceDetailedStatus_UPDATE_FAILED,
  OrganizationResourceDetailedStatus_UPDATE_IN_PROGRESS,
  OrganizationResourceDetailedStatus_UPDATE_SUCCESSFUL,
  OrganizationResourceDetailedStatus'
  #-}
