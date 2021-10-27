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
-- Module      : Network.AWS.AmplifyBackend.Types.ResolutionStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.ResolutionStrategy
  ( ResolutionStrategy
      ( ..,
        ResolutionStrategy_AUTOMERGE,
        ResolutionStrategy_LAMBDA,
        ResolutionStrategy_NONE,
        ResolutionStrategy_OPTIMISTIC_CONCURRENCY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResolutionStrategy = ResolutionStrategy'
  { fromResolutionStrategy ::
      Core.Text
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

pattern ResolutionStrategy_AUTOMERGE :: ResolutionStrategy
pattern ResolutionStrategy_AUTOMERGE = ResolutionStrategy' "AUTOMERGE"

pattern ResolutionStrategy_LAMBDA :: ResolutionStrategy
pattern ResolutionStrategy_LAMBDA = ResolutionStrategy' "LAMBDA"

pattern ResolutionStrategy_NONE :: ResolutionStrategy
pattern ResolutionStrategy_NONE = ResolutionStrategy' "NONE"

pattern ResolutionStrategy_OPTIMISTIC_CONCURRENCY :: ResolutionStrategy
pattern ResolutionStrategy_OPTIMISTIC_CONCURRENCY = ResolutionStrategy' "OPTIMISTIC_CONCURRENCY"

{-# COMPLETE
  ResolutionStrategy_AUTOMERGE,
  ResolutionStrategy_LAMBDA,
  ResolutionStrategy_NONE,
  ResolutionStrategy_OPTIMISTIC_CONCURRENCY,
  ResolutionStrategy'
  #-}
