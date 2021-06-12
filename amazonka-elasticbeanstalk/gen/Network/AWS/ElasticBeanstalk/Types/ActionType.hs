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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_InstanceRefresh,
        ActionType_PlatformUpdate,
        ActionType_Unknown
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionType = ActionType'
  { fromActionType ::
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

pattern ActionType_InstanceRefresh :: ActionType
pattern ActionType_InstanceRefresh = ActionType' "InstanceRefresh"

pattern ActionType_PlatformUpdate :: ActionType
pattern ActionType_PlatformUpdate = ActionType' "PlatformUpdate"

pattern ActionType_Unknown :: ActionType
pattern ActionType_Unknown = ActionType' "Unknown"

{-# COMPLETE
  ActionType_InstanceRefresh,
  ActionType_PlatformUpdate,
  ActionType_Unknown,
  ActionType'
  #-}
