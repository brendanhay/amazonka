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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.Status
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.Status
  ( Status
      ( ..,
        Status_DEPLOYED,
        Status_PENDING,
        Status_PENDING_DELETION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The deployment status of a resource. Status can be one of the following:
--
-- PENDING: Amazon Route 53 Application Recovery Controller is creating the
-- resource.
--
-- DEPLOYED: The resource is deployed and ready to use.
--
-- PENDING_DELETION: Amazon Route 53 Application Recovery Controller is
-- deleting the resource.
newtype Status = Status' {fromStatus :: Data.Text}
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

pattern Status_DEPLOYED :: Status
pattern Status_DEPLOYED = Status' "DEPLOYED"

pattern Status_PENDING :: Status
pattern Status_PENDING = Status' "PENDING"

pattern Status_PENDING_DELETION :: Status
pattern Status_PENDING_DELETION = Status' "PENDING_DELETION"

{-# COMPLETE
  Status_DEPLOYED,
  Status_PENDING,
  Status_PENDING_DELETION,
  Status'
  #-}
