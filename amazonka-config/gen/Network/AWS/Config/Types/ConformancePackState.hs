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

import qualified Network.AWS.Prelude as Prelude

newtype ConformancePackState = ConformancePackState'
  { fromConformancePackState ::
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
