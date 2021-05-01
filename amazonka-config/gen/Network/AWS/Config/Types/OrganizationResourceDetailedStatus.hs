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
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceDetailedStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype OrganizationResourceDetailedStatus = OrganizationResourceDetailedStatus'
  { fromOrganizationResourceDetailedStatus ::
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
