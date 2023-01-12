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
-- Module      : Amazonka.AppSync.Types.ConflictHandlerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ConflictHandlerType
  ( ConflictHandlerType
      ( ..,
        ConflictHandlerType_AUTOMERGE,
        ConflictHandlerType_LAMBDA,
        ConflictHandlerType_NONE,
        ConflictHandlerType_OPTIMISTIC_CONCURRENCY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConflictHandlerType = ConflictHandlerType'
  { fromConflictHandlerType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
