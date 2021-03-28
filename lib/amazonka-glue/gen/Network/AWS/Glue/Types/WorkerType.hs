{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.WorkerType
  ( WorkerType
    ( WorkerType'
    , WorkerTypeStandard
    , WorkerTypeG_1X
    , WorkerTypeG_2X
    , fromWorkerType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype WorkerType = WorkerType'{fromWorkerType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern WorkerTypeStandard :: WorkerType
pattern WorkerTypeStandard = WorkerType' "Standard"

pattern WorkerTypeG_1X :: WorkerType
pattern WorkerTypeG_1X = WorkerType' "G.1X"

pattern WorkerTypeG_2X :: WorkerType
pattern WorkerTypeG_2X = WorkerType' "G.2X"

{-# COMPLETE 
  WorkerTypeStandard,

  WorkerTypeG_1X,

  WorkerTypeG_2X,
  WorkerType'
  #-}
