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
-- Module      : Network.AWS.Translate.Types.ParallelDataStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataStatus
  ( ParallelDataStatus
      ( ..,
        ParallelDataStatus_ACTIVE,
        ParallelDataStatus_CREATING,
        ParallelDataStatus_DELETING,
        ParallelDataStatus_FAILED,
        ParallelDataStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ParallelDataStatus = ParallelDataStatus'
  { fromParallelDataStatus ::
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

pattern ParallelDataStatus_ACTIVE :: ParallelDataStatus
pattern ParallelDataStatus_ACTIVE = ParallelDataStatus' "ACTIVE"

pattern ParallelDataStatus_CREATING :: ParallelDataStatus
pattern ParallelDataStatus_CREATING = ParallelDataStatus' "CREATING"

pattern ParallelDataStatus_DELETING :: ParallelDataStatus
pattern ParallelDataStatus_DELETING = ParallelDataStatus' "DELETING"

pattern ParallelDataStatus_FAILED :: ParallelDataStatus
pattern ParallelDataStatus_FAILED = ParallelDataStatus' "FAILED"

pattern ParallelDataStatus_UPDATING :: ParallelDataStatus
pattern ParallelDataStatus_UPDATING = ParallelDataStatus' "UPDATING"

{-# COMPLETE
  ParallelDataStatus_ACTIVE,
  ParallelDataStatus_CREATING,
  ParallelDataStatus_DELETING,
  ParallelDataStatus_FAILED,
  ParallelDataStatus_UPDATING,
  ParallelDataStatus'
  #-}
