{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
  ( ServiceUpdateSeverity
      ( ServiceUpdateSeverity',
        ServiceUpdateSeverityCritical,
        ServiceUpdateSeverityImportant,
        ServiceUpdateSeverityMedium,
        ServiceUpdateSeverityLow,
        fromServiceUpdateSeverity
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ServiceUpdateSeverity = ServiceUpdateSeverity'
  { fromServiceUpdateSeverity ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ServiceUpdateSeverityCritical :: ServiceUpdateSeverity
pattern ServiceUpdateSeverityCritical = ServiceUpdateSeverity' "critical"

pattern ServiceUpdateSeverityImportant :: ServiceUpdateSeverity
pattern ServiceUpdateSeverityImportant = ServiceUpdateSeverity' "important"

pattern ServiceUpdateSeverityMedium :: ServiceUpdateSeverity
pattern ServiceUpdateSeverityMedium = ServiceUpdateSeverity' "medium"

pattern ServiceUpdateSeverityLow :: ServiceUpdateSeverity
pattern ServiceUpdateSeverityLow = ServiceUpdateSeverity' "low"

{-# COMPLETE
  ServiceUpdateSeverityCritical,
  ServiceUpdateSeverityImportant,
  ServiceUpdateSeverityMedium,
  ServiceUpdateSeverityLow,
  ServiceUpdateSeverity'
  #-}
