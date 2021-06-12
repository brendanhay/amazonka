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
-- Module      : Network.AWS.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackState
  ( ConformancePackState
      ( ..,
        ConformancePackState_CREATE_COMPLETE,
        ConformancePackState_CREATE_FAILED,
        ConformancePackState_CREATE_IN_PROGRESS,
        ConformancePackState_DELETE_FAILED,
        ConformancePackState_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConformancePackState = ConformancePackState'
  { fromConformancePackState ::
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

pattern ConformancePackState_CREATE_COMPLETE :: ConformancePackState
pattern ConformancePackState_CREATE_COMPLETE = ConformancePackState' "CREATE_COMPLETE"

pattern ConformancePackState_CREATE_FAILED :: ConformancePackState
pattern ConformancePackState_CREATE_FAILED = ConformancePackState' "CREATE_FAILED"

pattern ConformancePackState_CREATE_IN_PROGRESS :: ConformancePackState
pattern ConformancePackState_CREATE_IN_PROGRESS = ConformancePackState' "CREATE_IN_PROGRESS"

pattern ConformancePackState_DELETE_FAILED :: ConformancePackState
pattern ConformancePackState_DELETE_FAILED = ConformancePackState' "DELETE_FAILED"

pattern ConformancePackState_DELETE_IN_PROGRESS :: ConformancePackState
pattern ConformancePackState_DELETE_IN_PROGRESS = ConformancePackState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  ConformancePackState_CREATE_COMPLETE,
  ConformancePackState_CREATE_FAILED,
  ConformancePackState_CREATE_IN_PROGRESS,
  ConformancePackState_DELETE_FAILED,
  ConformancePackState_DELETE_IN_PROGRESS,
  ConformancePackState'
  #-}
