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
-- Module      : Amazonka.CloudFormation.Types.OrganizationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.OrganizationStatus
  ( OrganizationStatus
      ( ..,
        OrganizationStatus_DISABLED,
        OrganizationStatus_DISABLED_PERMANENTLY,
        OrganizationStatus_ENABLED
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

pattern OrganizationStatus_DISABLED :: OrganizationStatus
pattern OrganizationStatus_DISABLED = OrganizationStatus' "DISABLED"

pattern OrganizationStatus_DISABLED_PERMANENTLY :: OrganizationStatus
pattern OrganizationStatus_DISABLED_PERMANENTLY = OrganizationStatus' "DISABLED_PERMANENTLY"

pattern OrganizationStatus_ENABLED :: OrganizationStatus
pattern OrganizationStatus_ENABLED = OrganizationStatus' "ENABLED"

{-# COMPLETE
  OrganizationStatus_DISABLED,
  OrganizationStatus_DISABLED_PERMANENTLY,
  OrganizationStatus_ENABLED,
  OrganizationStatus'
  #-}
