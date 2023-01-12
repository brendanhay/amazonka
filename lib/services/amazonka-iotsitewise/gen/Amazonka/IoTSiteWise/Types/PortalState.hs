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
-- Module      : Amazonka.IoTSiteWise.Types.PortalState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PortalState
  ( PortalState
      ( ..,
        PortalState_ACTIVE,
        PortalState_CREATING,
        PortalState_DELETING,
        PortalState_FAILED,
        PortalState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PortalState = PortalState'
  { fromPortalState ::
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

pattern PortalState_ACTIVE :: PortalState
pattern PortalState_ACTIVE = PortalState' "ACTIVE"

pattern PortalState_CREATING :: PortalState
pattern PortalState_CREATING = PortalState' "CREATING"

pattern PortalState_DELETING :: PortalState
pattern PortalState_DELETING = PortalState' "DELETING"

pattern PortalState_FAILED :: PortalState
pattern PortalState_FAILED = PortalState' "FAILED"

pattern PortalState_UPDATING :: PortalState
pattern PortalState_UPDATING = PortalState' "UPDATING"

{-# COMPLETE
  PortalState_ACTIVE,
  PortalState_CREATING,
  PortalState_DELETING,
  PortalState_FAILED,
  PortalState_UPDATING,
  PortalState'
  #-}
