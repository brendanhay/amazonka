{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ConflictHandlerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ConflictHandlerType
  ( ConflictHandlerType
      ( ..,
        ConflictHandlerType_AUTOMERGE,
        ConflictHandlerType_LAMBDA,
        ConflictHandlerType_NONE,
        ConflictHandlerType_OPTIMISTIC_CONCURRENCY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConflictHandlerType = ConflictHandlerType'
  { fromConflictHandlerType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ConflictHandlerType_AUTOMERGE :: ConflictHandlerType
pattern ConflictHandlerType_AUTOMERGE = ConflictHandlerType' "AUTOMERGE"

pattern ConflictHandlerType_LAMBDA :: ConflictHandlerType
pattern ConflictHandlerType_LAMBDA = ConflictHandlerType' "LAMBDA"

pattern ConflictHandlerType_NONE :: ConflictHandlerType
pattern ConflictHandlerType_NONE = ConflictHandlerType' "NONE"

pattern ConflictHandlerType_OPTIMISTIC_CONCURRENCY :: ConflictHandlerType
pattern ConflictHandlerType_OPTIMISTIC_CONCURRENCY = ConflictHandlerType' "OPTIMISTIC_CONCURRENCY"

{-# COMPLETE
  ConflictHandlerType_AUTOMERGE,
  ConflictHandlerType_LAMBDA,
  ConflictHandlerType_NONE,
  ConflictHandlerType_OPTIMISTIC_CONCURRENCY,
  ConflictHandlerType'
  #-}
