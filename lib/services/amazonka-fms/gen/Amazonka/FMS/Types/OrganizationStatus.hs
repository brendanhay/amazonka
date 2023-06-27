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
-- Module      : Amazonka.FMS.Types.OrganizationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.OrganizationStatus
  ( OrganizationStatus
      ( ..,
        OrganizationStatus_OFFBOARDING,
        OrganizationStatus_OFFBOARDING_COMPLETE,
        OrganizationStatus_ONBOARDING,
        OrganizationStatus_ONBOARDING_COMPLETE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrganizationStatus = OrganizationStatus'
  { fromOrganizationStatus ::
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

pattern OrganizationStatus_OFFBOARDING :: OrganizationStatus
pattern OrganizationStatus_OFFBOARDING = OrganizationStatus' "OFFBOARDING"

pattern OrganizationStatus_OFFBOARDING_COMPLETE :: OrganizationStatus
pattern OrganizationStatus_OFFBOARDING_COMPLETE = OrganizationStatus' "OFFBOARDING_COMPLETE"

pattern OrganizationStatus_ONBOARDING :: OrganizationStatus
pattern OrganizationStatus_ONBOARDING = OrganizationStatus' "ONBOARDING"

pattern OrganizationStatus_ONBOARDING_COMPLETE :: OrganizationStatus
pattern OrganizationStatus_ONBOARDING_COMPLETE = OrganizationStatus' "ONBOARDING_COMPLETE"

{-# COMPLETE
  OrganizationStatus_OFFBOARDING,
  OrganizationStatus_OFFBOARDING_COMPLETE,
  OrganizationStatus_ONBOARDING,
  OrganizationStatus_ONBOARDING_COMPLETE,
  OrganizationStatus'
  #-}
