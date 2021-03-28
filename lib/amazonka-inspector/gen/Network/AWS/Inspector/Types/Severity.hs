{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Severity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Severity
  ( Severity
    ( Severity'
    , SeverityLow
    , SeverityMedium
    , SeverityHigh
    , SeverityInformational
    , SeverityUndefined
    , fromSeverity
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Severity = Severity'{fromSeverity :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern SeverityLow :: Severity
pattern SeverityLow = Severity' "Low"

pattern SeverityMedium :: Severity
pattern SeverityMedium = Severity' "Medium"

pattern SeverityHigh :: Severity
pattern SeverityHigh = Severity' "High"

pattern SeverityInformational :: Severity
pattern SeverityInformational = Severity' "Informational"

pattern SeverityUndefined :: Severity
pattern SeverityUndefined = Severity' "Undefined"

{-# COMPLETE 
  SeverityLow,

  SeverityMedium,

  SeverityHigh,

  SeverityInformational,

  SeverityUndefined,
  Severity'
  #-}
