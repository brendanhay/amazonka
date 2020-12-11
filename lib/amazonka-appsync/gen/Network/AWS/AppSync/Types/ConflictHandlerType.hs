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
        CHTAutomerge,
        CHTLambda,
        CHTNone,
        CHTOptimisticConcurrency
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConflictHandlerType = ConflictHandlerType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CHTAutomerge :: ConflictHandlerType
pattern CHTAutomerge = ConflictHandlerType' "AUTOMERGE"

pattern CHTLambda :: ConflictHandlerType
pattern CHTLambda = ConflictHandlerType' "LAMBDA"

pattern CHTNone :: ConflictHandlerType
pattern CHTNone = ConflictHandlerType' "NONE"

pattern CHTOptimisticConcurrency :: ConflictHandlerType
pattern CHTOptimisticConcurrency = ConflictHandlerType' "OPTIMISTIC_CONCURRENCY"

{-# COMPLETE
  CHTAutomerge,
  CHTLambda,
  CHTNone,
  CHTOptimisticConcurrency,
  ConflictHandlerType'
  #-}
