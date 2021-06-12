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
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
  ( ConflictResolutionStrategyTypeEnum
      ( ..,
        ConflictResolutionStrategyTypeEnum_ACCEPT_DESTINATION,
        ConflictResolutionStrategyTypeEnum_ACCEPT_SOURCE,
        ConflictResolutionStrategyTypeEnum_AUTOMERGE,
        ConflictResolutionStrategyTypeEnum_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConflictResolutionStrategyTypeEnum = ConflictResolutionStrategyTypeEnum'
  { fromConflictResolutionStrategyTypeEnum ::
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

pattern ConflictResolutionStrategyTypeEnum_ACCEPT_DESTINATION :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnum_ACCEPT_DESTINATION = ConflictResolutionStrategyTypeEnum' "ACCEPT_DESTINATION"

pattern ConflictResolutionStrategyTypeEnum_ACCEPT_SOURCE :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnum_ACCEPT_SOURCE = ConflictResolutionStrategyTypeEnum' "ACCEPT_SOURCE"

pattern ConflictResolutionStrategyTypeEnum_AUTOMERGE :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnum_AUTOMERGE = ConflictResolutionStrategyTypeEnum' "AUTOMERGE"

pattern ConflictResolutionStrategyTypeEnum_NONE :: ConflictResolutionStrategyTypeEnum
pattern ConflictResolutionStrategyTypeEnum_NONE = ConflictResolutionStrategyTypeEnum' "NONE"

{-# COMPLETE
  ConflictResolutionStrategyTypeEnum_ACCEPT_DESTINATION,
  ConflictResolutionStrategyTypeEnum_ACCEPT_SOURCE,
  ConflictResolutionStrategyTypeEnum_AUTOMERGE,
  ConflictResolutionStrategyTypeEnum_NONE,
  ConflictResolutionStrategyTypeEnum'
  #-}
