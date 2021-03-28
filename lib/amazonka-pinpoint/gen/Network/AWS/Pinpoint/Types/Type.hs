{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Type
  ( Type
    ( Type'
    , TypeAll
    , TypeAny
    , TypeNone
    , fromType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Type = Type'{fromType :: Core.Text}
                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                 Core.Generic)
                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                   Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                   Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern TypeAll :: Type
pattern TypeAll = Type' "ALL"

pattern TypeAny :: Type
pattern TypeAny = Type' "ANY"

pattern TypeNone :: Type
pattern TypeNone = Type' "NONE"

{-# COMPLETE 
  TypeAll,

  TypeAny,

  TypeNone,
  Type'
  #-}
