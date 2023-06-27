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
-- Module      : Amazonka.MGN.Types.ActionCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ActionCategory
  ( ActionCategory
      ( ..,
        ActionCategory_BACKUP,
        ActionCategory_CONFIGURATION,
        ActionCategory_DISASTER_RECOVERY,
        ActionCategory_LICENSE_AND_SUBSCRIPTION,
        ActionCategory_NETWORKING,
        ActionCategory_OBSERVABILITY,
        ActionCategory_OPERATING_SYSTEM,
        ActionCategory_OTHER,
        ActionCategory_SECURITY,
        ActionCategory_VALIDATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionCategory = ActionCategory'
  { fromActionCategory ::
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

pattern ActionCategory_BACKUP :: ActionCategory
pattern ActionCategory_BACKUP = ActionCategory' "BACKUP"

pattern ActionCategory_CONFIGURATION :: ActionCategory
pattern ActionCategory_CONFIGURATION = ActionCategory' "CONFIGURATION"

pattern ActionCategory_DISASTER_RECOVERY :: ActionCategory
pattern ActionCategory_DISASTER_RECOVERY = ActionCategory' "DISASTER_RECOVERY"

pattern ActionCategory_LICENSE_AND_SUBSCRIPTION :: ActionCategory
pattern ActionCategory_LICENSE_AND_SUBSCRIPTION = ActionCategory' "LICENSE_AND_SUBSCRIPTION"

pattern ActionCategory_NETWORKING :: ActionCategory
pattern ActionCategory_NETWORKING = ActionCategory' "NETWORKING"

pattern ActionCategory_OBSERVABILITY :: ActionCategory
pattern ActionCategory_OBSERVABILITY = ActionCategory' "OBSERVABILITY"

pattern ActionCategory_OPERATING_SYSTEM :: ActionCategory
pattern ActionCategory_OPERATING_SYSTEM = ActionCategory' "OPERATING_SYSTEM"

pattern ActionCategory_OTHER :: ActionCategory
pattern ActionCategory_OTHER = ActionCategory' "OTHER"

pattern ActionCategory_SECURITY :: ActionCategory
pattern ActionCategory_SECURITY = ActionCategory' "SECURITY"

pattern ActionCategory_VALIDATION :: ActionCategory
pattern ActionCategory_VALIDATION = ActionCategory' "VALIDATION"

{-# COMPLETE
  ActionCategory_BACKUP,
  ActionCategory_CONFIGURATION,
  ActionCategory_DISASTER_RECOVERY,
  ActionCategory_LICENSE_AND_SUBSCRIPTION,
  ActionCategory_NETWORKING,
  ActionCategory_OBSERVABILITY,
  ActionCategory_OPERATING_SYSTEM,
  ActionCategory_OTHER,
  ActionCategory_SECURITY,
  ActionCategory_VALIDATION,
  ActionCategory'
  #-}
