{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Architecture
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Architecture
  ( Architecture
    ( Architecture'
    , ArchitectureX8664
    , ArchitectureI386
    , fromArchitecture
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Architecture = Architecture'{fromArchitecture :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ArchitectureX8664 :: Architecture
pattern ArchitectureX8664 = Architecture' "x86_64"

pattern ArchitectureI386 :: Architecture
pattern ArchitectureI386 = Architecture' "i386"

{-# COMPLETE 
  ArchitectureX8664,

  ArchitectureI386,
  Architecture'
  #-}
