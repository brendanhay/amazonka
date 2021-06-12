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
-- Module      : Network.AWS.Batch.Types.JQStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JQStatus
  ( JQStatus
      ( ..,
        JQStatus_CREATING,
        JQStatus_DELETED,
        JQStatus_DELETING,
        JQStatus_INVALID,
        JQStatus_UPDATING,
        JQStatus_VALID
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype JQStatus = JQStatus'
  { fromJQStatus ::
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

pattern JQStatus_CREATING :: JQStatus
pattern JQStatus_CREATING = JQStatus' "CREATING"

pattern JQStatus_DELETED :: JQStatus
pattern JQStatus_DELETED = JQStatus' "DELETED"

pattern JQStatus_DELETING :: JQStatus
pattern JQStatus_DELETING = JQStatus' "DELETING"

pattern JQStatus_INVALID :: JQStatus
pattern JQStatus_INVALID = JQStatus' "INVALID"

pattern JQStatus_UPDATING :: JQStatus
pattern JQStatus_UPDATING = JQStatus' "UPDATING"

pattern JQStatus_VALID :: JQStatus
pattern JQStatus_VALID = JQStatus' "VALID"

{-# COMPLETE
  JQStatus_CREATING,
  JQStatus_DELETED,
  JQStatus_DELETING,
  JQStatus_INVALID,
  JQStatus_UPDATING,
  JQStatus_VALID,
  JQStatus'
  #-}
