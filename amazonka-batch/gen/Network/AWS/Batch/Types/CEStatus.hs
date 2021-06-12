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
-- Module      : Network.AWS.Batch.Types.CEStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CEStatus
  ( CEStatus
      ( ..,
        CEStatus_CREATING,
        CEStatus_DELETED,
        CEStatus_DELETING,
        CEStatus_INVALID,
        CEStatus_UPDATING,
        CEStatus_VALID
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CEStatus = CEStatus'
  { fromCEStatus ::
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

pattern CEStatus_CREATING :: CEStatus
pattern CEStatus_CREATING = CEStatus' "CREATING"

pattern CEStatus_DELETED :: CEStatus
pattern CEStatus_DELETED = CEStatus' "DELETED"

pattern CEStatus_DELETING :: CEStatus
pattern CEStatus_DELETING = CEStatus' "DELETING"

pattern CEStatus_INVALID :: CEStatus
pattern CEStatus_INVALID = CEStatus' "INVALID"

pattern CEStatus_UPDATING :: CEStatus
pattern CEStatus_UPDATING = CEStatus' "UPDATING"

pattern CEStatus_VALID :: CEStatus
pattern CEStatus_VALID = CEStatus' "VALID"

{-# COMPLETE
  CEStatus_CREATING,
  CEStatus_DELETED,
  CEStatus_DELETING,
  CEStatus_INVALID,
  CEStatus_UPDATING,
  CEStatus_VALID,
  CEStatus'
  #-}
