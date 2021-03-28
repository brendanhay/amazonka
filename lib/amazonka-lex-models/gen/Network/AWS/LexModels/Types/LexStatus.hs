{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LexStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.LexStatus
  ( LexStatus
    ( LexStatus'
    , LexStatusBuilding
    , LexStatusReady
    , LexStatusReadyBasicTesting
    , LexStatusFailed
    , LexStatusNotBuilt
    , fromLexStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LexStatus = LexStatus'{fromLexStatus :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern LexStatusBuilding :: LexStatus
pattern LexStatusBuilding = LexStatus' "BUILDING"

pattern LexStatusReady :: LexStatus
pattern LexStatusReady = LexStatus' "READY"

pattern LexStatusReadyBasicTesting :: LexStatus
pattern LexStatusReadyBasicTesting = LexStatus' "READY_BASIC_TESTING"

pattern LexStatusFailed :: LexStatus
pattern LexStatusFailed = LexStatus' "FAILED"

pattern LexStatusNotBuilt :: LexStatus
pattern LexStatusNotBuilt = LexStatus' "NOT_BUILT"

{-# COMPLETE 
  LexStatusBuilding,

  LexStatusReady,

  LexStatusReadyBasicTesting,

  LexStatusFailed,

  LexStatusNotBuilt,
  LexStatus'
  #-}
