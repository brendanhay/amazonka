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
-- Module      : Amazonka.MacieV2.Types.AdminStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AdminStatus
  ( AdminStatus
      ( ..,
        AdminStatus_DISABLING_IN_PROGRESS,
        AdminStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of an account as the delegated Amazon Macie
-- administrator account for an organization in Organizations. Possible
-- values are:
newtype AdminStatus = AdminStatus'
  { fromAdminStatus ::
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

pattern AdminStatus_DISABLING_IN_PROGRESS :: AdminStatus
pattern AdminStatus_DISABLING_IN_PROGRESS = AdminStatus' "DISABLING_IN_PROGRESS"

pattern AdminStatus_ENABLED :: AdminStatus
pattern AdminStatus_ENABLED = AdminStatus' "ENABLED"

{-# COMPLETE
  AdminStatus_DISABLING_IN_PROGRESS,
  AdminStatus_ENABLED,
  AdminStatus'
  #-}
