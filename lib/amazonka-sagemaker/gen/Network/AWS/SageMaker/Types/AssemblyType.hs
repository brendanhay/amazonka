{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AssemblyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AssemblyType
  ( AssemblyType
    ( AssemblyType'
    , AssemblyTypeNone
    , AssemblyTypeLine
    , fromAssemblyType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AssemblyType = AssemblyType'{fromAssemblyType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern AssemblyTypeNone :: AssemblyType
pattern AssemblyTypeNone = AssemblyType' "None"

pattern AssemblyTypeLine :: AssemblyType
pattern AssemblyTypeLine = AssemblyType' "Line"

{-# COMPLETE 
  AssemblyTypeNone,

  AssemblyTypeLine,
  AssemblyType'
  #-}
