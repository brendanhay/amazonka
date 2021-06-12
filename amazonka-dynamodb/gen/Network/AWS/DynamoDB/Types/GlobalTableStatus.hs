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
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableStatus
  ( GlobalTableStatus
      ( ..,
        GlobalTableStatus_ACTIVE,
        GlobalTableStatus_CREATING,
        GlobalTableStatus_DELETING,
        GlobalTableStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GlobalTableStatus = GlobalTableStatus'
  { fromGlobalTableStatus ::
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

pattern GlobalTableStatus_ACTIVE :: GlobalTableStatus
pattern GlobalTableStatus_ACTIVE = GlobalTableStatus' "ACTIVE"

pattern GlobalTableStatus_CREATING :: GlobalTableStatus
pattern GlobalTableStatus_CREATING = GlobalTableStatus' "CREATING"

pattern GlobalTableStatus_DELETING :: GlobalTableStatus
pattern GlobalTableStatus_DELETING = GlobalTableStatus' "DELETING"

pattern GlobalTableStatus_UPDATING :: GlobalTableStatus
pattern GlobalTableStatus_UPDATING = GlobalTableStatus' "UPDATING"

{-# COMPLETE
  GlobalTableStatus_ACTIVE,
  GlobalTableStatus_CREATING,
  GlobalTableStatus_DELETING,
  GlobalTableStatus_UPDATING,
  GlobalTableStatus'
  #-}
