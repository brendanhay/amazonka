{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ArchitectureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ArchitectureType
  ( ArchitectureType
    ( ArchitectureType'
    , ArchitectureTypeI386
    , ArchitectureTypeX8664
    , ArchitectureTypeARM64
    , fromArchitectureType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ArchitectureType = ArchitectureType'{fromArchitectureType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern ArchitectureTypeI386 :: ArchitectureType
pattern ArchitectureTypeI386 = ArchitectureType' "i386"

pattern ArchitectureTypeX8664 :: ArchitectureType
pattern ArchitectureTypeX8664 = ArchitectureType' "x86_64"

pattern ArchitectureTypeARM64 :: ArchitectureType
pattern ArchitectureTypeARM64 = ArchitectureType' "arm64"

{-# COMPLETE 
  ArchitectureTypeI386,

  ArchitectureTypeX8664,

  ArchitectureTypeARM64,
  ArchitectureType'
  #-}
