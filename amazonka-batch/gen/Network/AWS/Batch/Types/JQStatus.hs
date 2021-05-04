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

import qualified Network.AWS.Prelude as Prelude

newtype JQStatus = JQStatus'
  { fromJQStatus ::
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
