{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ConflictHandlerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ConflictHandlerType
  ( ConflictHandlerType
      ( ConflictHandlerType',
        ConflictHandlerTypeOptimisticConcurrency,
        ConflictHandlerTypeLambda,
        ConflictHandlerTypeAutomerge,
        ConflictHandlerTypeNone,
        fromConflictHandlerType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConflictHandlerType = ConflictHandlerType'
  { fromConflictHandlerType ::
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

pattern ConflictHandlerTypeOptimisticConcurrency :: ConflictHandlerType
pattern ConflictHandlerTypeOptimisticConcurrency = ConflictHandlerType' "OPTIMISTIC_CONCURRENCY"

pattern ConflictHandlerTypeLambda :: ConflictHandlerType
pattern ConflictHandlerTypeLambda = ConflictHandlerType' "LAMBDA"

pattern ConflictHandlerTypeAutomerge :: ConflictHandlerType
pattern ConflictHandlerTypeAutomerge = ConflictHandlerType' "AUTOMERGE"

pattern ConflictHandlerTypeNone :: ConflictHandlerType
pattern ConflictHandlerTypeNone = ConflictHandlerType' "NONE"

{-# COMPLETE
  ConflictHandlerTypeOptimisticConcurrency,
  ConflictHandlerTypeLambda,
  ConflictHandlerTypeAutomerge,
  ConflictHandlerTypeNone,
  ConflictHandlerType'
  #-}
