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
-- Module      : Network.AWS.DynamoDB.Types.IndexStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.IndexStatus
  ( IndexStatus
      ( ..,
        IndexStatus_ACTIVE,
        IndexStatus_CREATING,
        IndexStatus_DELETING,
        IndexStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype IndexStatus = IndexStatus'
  { fromIndexStatus ::
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

pattern IndexStatus_ACTIVE :: IndexStatus
pattern IndexStatus_ACTIVE = IndexStatus' "ACTIVE"

pattern IndexStatus_CREATING :: IndexStatus
pattern IndexStatus_CREATING = IndexStatus' "CREATING"

pattern IndexStatus_DELETING :: IndexStatus
pattern IndexStatus_DELETING = IndexStatus' "DELETING"

pattern IndexStatus_UPDATING :: IndexStatus
pattern IndexStatus_UPDATING = IndexStatus' "UPDATING"

{-# COMPLETE
  IndexStatus_ACTIVE,
  IndexStatus_CREATING,
  IndexStatus_DELETING,
  IndexStatus_UPDATING,
  IndexStatus'
  #-}
