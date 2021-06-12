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
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingStatusType
  ( ScalingStatusType
      ( ..,
        ScalingStatusType_ACTIVE,
        ScalingStatusType_DELETED,
        ScalingStatusType_DELETE_REQUESTED,
        ScalingStatusType_DELETING,
        ScalingStatusType_ERROR,
        ScalingStatusType_UPDATE_REQUESTED,
        ScalingStatusType_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScalingStatusType = ScalingStatusType'
  { fromScalingStatusType ::
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

pattern ScalingStatusType_ACTIVE :: ScalingStatusType
pattern ScalingStatusType_ACTIVE = ScalingStatusType' "ACTIVE"

pattern ScalingStatusType_DELETED :: ScalingStatusType
pattern ScalingStatusType_DELETED = ScalingStatusType' "DELETED"

pattern ScalingStatusType_DELETE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_DELETE_REQUESTED = ScalingStatusType' "DELETE_REQUESTED"

pattern ScalingStatusType_DELETING :: ScalingStatusType
pattern ScalingStatusType_DELETING = ScalingStatusType' "DELETING"

pattern ScalingStatusType_ERROR :: ScalingStatusType
pattern ScalingStatusType_ERROR = ScalingStatusType' "ERROR"

pattern ScalingStatusType_UPDATE_REQUESTED :: ScalingStatusType
pattern ScalingStatusType_UPDATE_REQUESTED = ScalingStatusType' "UPDATE_REQUESTED"

pattern ScalingStatusType_UPDATING :: ScalingStatusType
pattern ScalingStatusType_UPDATING = ScalingStatusType' "UPDATING"

{-# COMPLETE
  ScalingStatusType_ACTIVE,
  ScalingStatusType_DELETED,
  ScalingStatusType_DELETE_REQUESTED,
  ScalingStatusType_DELETING,
  ScalingStatusType_ERROR,
  ScalingStatusType_UPDATE_REQUESTED,
  ScalingStatusType_UPDATING,
  ScalingStatusType'
  #-}
