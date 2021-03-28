{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResettableElementName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.ResettableElementName
  ( ResettableElementName
    ( ResettableElementName'
    , ResettableElementNameFullyQualifiedDomainName
    , ResettableElementNameRegions
    , ResettableElementNameResourcePath
    , ResettableElementNameChildHealthChecks
    , fromResettableElementName
    )
  ) where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types

newtype ResettableElementName = ResettableElementName'{fromResettableElementName
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ResettableElementNameFullyQualifiedDomainName :: ResettableElementName
pattern ResettableElementNameFullyQualifiedDomainName = ResettableElementName' "FullyQualifiedDomainName"

pattern ResettableElementNameRegions :: ResettableElementName
pattern ResettableElementNameRegions = ResettableElementName' "Regions"

pattern ResettableElementNameResourcePath :: ResettableElementName
pattern ResettableElementNameResourcePath = ResettableElementName' "ResourcePath"

pattern ResettableElementNameChildHealthChecks :: ResettableElementName
pattern ResettableElementNameChildHealthChecks = ResettableElementName' "ChildHealthChecks"

{-# COMPLETE 
  ResettableElementNameFullyQualifiedDomainName,

  ResettableElementNameRegions,

  ResettableElementNameResourcePath,

  ResettableElementNameChildHealthChecks,
  ResettableElementName'
  #-}
