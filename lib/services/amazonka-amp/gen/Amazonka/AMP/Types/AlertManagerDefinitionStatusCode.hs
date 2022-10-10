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
-- Module      : Amazonka.AMP.Types.AlertManagerDefinitionStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.AlertManagerDefinitionStatusCode
  ( AlertManagerDefinitionStatusCode
      ( ..,
        AlertManagerDefinitionStatusCode_ACTIVE,
        AlertManagerDefinitionStatusCode_CREATING,
        AlertManagerDefinitionStatusCode_CREATION_FAILED,
        AlertManagerDefinitionStatusCode_DELETING,
        AlertManagerDefinitionStatusCode_UPDATE_FAILED,
        AlertManagerDefinitionStatusCode_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | State of an alert manager definition.
newtype AlertManagerDefinitionStatusCode = AlertManagerDefinitionStatusCode'
  { fromAlertManagerDefinitionStatusCode ::
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

pattern AlertManagerDefinitionStatusCode_ACTIVE :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_ACTIVE = AlertManagerDefinitionStatusCode' "ACTIVE"

pattern AlertManagerDefinitionStatusCode_CREATING :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_CREATING = AlertManagerDefinitionStatusCode' "CREATING"

pattern AlertManagerDefinitionStatusCode_CREATION_FAILED :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_CREATION_FAILED = AlertManagerDefinitionStatusCode' "CREATION_FAILED"

pattern AlertManagerDefinitionStatusCode_DELETING :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_DELETING = AlertManagerDefinitionStatusCode' "DELETING"

pattern AlertManagerDefinitionStatusCode_UPDATE_FAILED :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_UPDATE_FAILED = AlertManagerDefinitionStatusCode' "UPDATE_FAILED"

pattern AlertManagerDefinitionStatusCode_UPDATING :: AlertManagerDefinitionStatusCode
pattern AlertManagerDefinitionStatusCode_UPDATING = AlertManagerDefinitionStatusCode' "UPDATING"

{-# COMPLETE
  AlertManagerDefinitionStatusCode_ACTIVE,
  AlertManagerDefinitionStatusCode_CREATING,
  AlertManagerDefinitionStatusCode_CREATION_FAILED,
  AlertManagerDefinitionStatusCode_DELETING,
  AlertManagerDefinitionStatusCode_UPDATE_FAILED,
  AlertManagerDefinitionStatusCode_UPDATING,
  AlertManagerDefinitionStatusCode'
  #-}
