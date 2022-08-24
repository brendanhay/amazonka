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
-- Module      : Amazonka.WellArchitected.Types.OrganizationSharingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.OrganizationSharingStatus
  ( OrganizationSharingStatus
      ( ..,
        OrganizationSharingStatus_DISABLED,
        OrganizationSharingStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OrganizationSharingStatus = OrganizationSharingStatus'
  { fromOrganizationSharingStatus ::
      Core.Text
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

pattern OrganizationSharingStatus_DISABLED :: OrganizationSharingStatus
pattern OrganizationSharingStatus_DISABLED = OrganizationSharingStatus' "DISABLED"

pattern OrganizationSharingStatus_ENABLED :: OrganizationSharingStatus
pattern OrganizationSharingStatus_ENABLED = OrganizationSharingStatus' "ENABLED"

{-# COMPLETE
  OrganizationSharingStatus_DISABLED,
  OrganizationSharingStatus_ENABLED,
  OrganizationSharingStatus'
  #-}
