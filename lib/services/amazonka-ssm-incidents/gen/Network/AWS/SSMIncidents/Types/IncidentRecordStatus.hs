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
-- Module      : Network.AWS.SSMIncidents.Types.IncidentRecordStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.IncidentRecordStatus
  ( IncidentRecordStatus
      ( ..,
        IncidentRecordStatus_OPEN,
        IncidentRecordStatus_RESOLVED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype IncidentRecordStatus = IncidentRecordStatus'
  { fromIncidentRecordStatus ::
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

pattern IncidentRecordStatus_OPEN :: IncidentRecordStatus
pattern IncidentRecordStatus_OPEN = IncidentRecordStatus' "OPEN"

pattern IncidentRecordStatus_RESOLVED :: IncidentRecordStatus
pattern IncidentRecordStatus_RESOLVED = IncidentRecordStatus' "RESOLVED"

{-# COMPLETE
  IncidentRecordStatus_OPEN,
  IncidentRecordStatus_RESOLVED,
  IncidentRecordStatus'
  #-}
