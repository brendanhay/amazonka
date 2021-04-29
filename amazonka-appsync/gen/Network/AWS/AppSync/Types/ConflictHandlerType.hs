{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ConflictHandlerType = ConflictHandlerType'
  { fromConflictHandlerType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
