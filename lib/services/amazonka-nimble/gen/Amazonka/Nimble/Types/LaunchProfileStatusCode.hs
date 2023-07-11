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
-- Module      : Amazonka.Nimble.Types.LaunchProfileStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileStatusCode
  ( LaunchProfileStatusCode
      ( ..,
        LaunchProfileStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
        LaunchProfileStatusCode_ENCRYPTION_KEY_NOT_FOUND,
        LaunchProfileStatusCode_INTERNAL_ERROR,
        LaunchProfileStatusCode_INVALID_INSTANCE_TYPES_PROVIDED,
        LaunchProfileStatusCode_INVALID_SUBNETS_COMBINATION,
        LaunchProfileStatusCode_INVALID_SUBNETS_PROVIDED,
        LaunchProfileStatusCode_LAUNCH_PROFILE_CREATED,
        LaunchProfileStatusCode_LAUNCH_PROFILE_CREATE_IN_PROGRESS,
        LaunchProfileStatusCode_LAUNCH_PROFILE_DELETED,
        LaunchProfileStatusCode_LAUNCH_PROFILE_DELETE_IN_PROGRESS,
        LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATED,
        LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATE_IN_PROGRESS,
        LaunchProfileStatusCode_LAUNCH_PROFILE_WITH_STREAM_SESSIONS_NOT_DELETED,
        LaunchProfileStatusCode_STREAMING_IMAGE_NOT_FOUND,
        LaunchProfileStatusCode_STREAMING_IMAGE_NOT_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfileStatusCode = LaunchProfileStatusCode'
  { fromLaunchProfileStatusCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern LaunchProfileStatusCode_ENCRYPTION_KEY_ACCESS_DENIED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_ENCRYPTION_KEY_ACCESS_DENIED = LaunchProfileStatusCode' "ENCRYPTION_KEY_ACCESS_DENIED"

pattern LaunchProfileStatusCode_ENCRYPTION_KEY_NOT_FOUND :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_ENCRYPTION_KEY_NOT_FOUND = LaunchProfileStatusCode' "ENCRYPTION_KEY_NOT_FOUND"

pattern LaunchProfileStatusCode_INTERNAL_ERROR :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_INTERNAL_ERROR = LaunchProfileStatusCode' "INTERNAL_ERROR"

pattern LaunchProfileStatusCode_INVALID_INSTANCE_TYPES_PROVIDED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_INVALID_INSTANCE_TYPES_PROVIDED = LaunchProfileStatusCode' "INVALID_INSTANCE_TYPES_PROVIDED"

pattern LaunchProfileStatusCode_INVALID_SUBNETS_COMBINATION :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_INVALID_SUBNETS_COMBINATION = LaunchProfileStatusCode' "INVALID_SUBNETS_COMBINATION"

pattern LaunchProfileStatusCode_INVALID_SUBNETS_PROVIDED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_INVALID_SUBNETS_PROVIDED = LaunchProfileStatusCode' "INVALID_SUBNETS_PROVIDED"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_CREATED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_CREATED = LaunchProfileStatusCode' "LAUNCH_PROFILE_CREATED"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_CREATE_IN_PROGRESS :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_CREATE_IN_PROGRESS = LaunchProfileStatusCode' "LAUNCH_PROFILE_CREATE_IN_PROGRESS"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_DELETED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_DELETED = LaunchProfileStatusCode' "LAUNCH_PROFILE_DELETED"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_DELETE_IN_PROGRESS :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_DELETE_IN_PROGRESS = LaunchProfileStatusCode' "LAUNCH_PROFILE_DELETE_IN_PROGRESS"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATED = LaunchProfileStatusCode' "LAUNCH_PROFILE_UPDATED"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATE_IN_PROGRESS :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATE_IN_PROGRESS = LaunchProfileStatusCode' "LAUNCH_PROFILE_UPDATE_IN_PROGRESS"

pattern LaunchProfileStatusCode_LAUNCH_PROFILE_WITH_STREAM_SESSIONS_NOT_DELETED :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_LAUNCH_PROFILE_WITH_STREAM_SESSIONS_NOT_DELETED = LaunchProfileStatusCode' "LAUNCH_PROFILE_WITH_STREAM_SESSIONS_NOT_DELETED"

pattern LaunchProfileStatusCode_STREAMING_IMAGE_NOT_FOUND :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_STREAMING_IMAGE_NOT_FOUND = LaunchProfileStatusCode' "STREAMING_IMAGE_NOT_FOUND"

pattern LaunchProfileStatusCode_STREAMING_IMAGE_NOT_READY :: LaunchProfileStatusCode
pattern LaunchProfileStatusCode_STREAMING_IMAGE_NOT_READY = LaunchProfileStatusCode' "STREAMING_IMAGE_NOT_READY"

{-# COMPLETE
  LaunchProfileStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
  LaunchProfileStatusCode_ENCRYPTION_KEY_NOT_FOUND,
  LaunchProfileStatusCode_INTERNAL_ERROR,
  LaunchProfileStatusCode_INVALID_INSTANCE_TYPES_PROVIDED,
  LaunchProfileStatusCode_INVALID_SUBNETS_COMBINATION,
  LaunchProfileStatusCode_INVALID_SUBNETS_PROVIDED,
  LaunchProfileStatusCode_LAUNCH_PROFILE_CREATED,
  LaunchProfileStatusCode_LAUNCH_PROFILE_CREATE_IN_PROGRESS,
  LaunchProfileStatusCode_LAUNCH_PROFILE_DELETED,
  LaunchProfileStatusCode_LAUNCH_PROFILE_DELETE_IN_PROGRESS,
  LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATED,
  LaunchProfileStatusCode_LAUNCH_PROFILE_UPDATE_IN_PROGRESS,
  LaunchProfileStatusCode_LAUNCH_PROFILE_WITH_STREAM_SESSIONS_NOT_DELETED,
  LaunchProfileStatusCode_STREAMING_IMAGE_NOT_FOUND,
  LaunchProfileStatusCode_STREAMING_IMAGE_NOT_READY,
  LaunchProfileStatusCode'
  #-}
