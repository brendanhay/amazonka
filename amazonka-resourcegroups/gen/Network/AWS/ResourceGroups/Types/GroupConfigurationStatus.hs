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
-- Module      : Network.AWS.ResourceGroups.Types.GroupConfigurationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupConfigurationStatus
  ( GroupConfigurationStatus
      ( ..,
        GroupConfigurationStatus_UPDATE_COMPLETE,
        GroupConfigurationStatus_UPDATE_FAILED,
        GroupConfigurationStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype GroupConfigurationStatus = GroupConfigurationStatus'
  { fromGroupConfigurationStatus ::
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

pattern GroupConfigurationStatus_UPDATE_COMPLETE :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATE_COMPLETE = GroupConfigurationStatus' "UPDATE_COMPLETE"

pattern GroupConfigurationStatus_UPDATE_FAILED :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATE_FAILED = GroupConfigurationStatus' "UPDATE_FAILED"

pattern GroupConfigurationStatus_UPDATING :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATING = GroupConfigurationStatus' "UPDATING"

{-# COMPLETE
  GroupConfigurationStatus_UPDATE_COMPLETE,
  GroupConfigurationStatus_UPDATE_FAILED,
  GroupConfigurationStatus_UPDATING,
  GroupConfigurationStatus'
  #-}
