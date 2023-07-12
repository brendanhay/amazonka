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
-- Module      : Amazonka.Nimble.Types.StudioStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioStatusCode
  ( StudioStatusCode
      ( ..,
        StudioStatusCode_AWS_SSO_ACCESS_DENIED,
        StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIRED,
        StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS,
        StudioStatusCode_AWS_SSO_NOT_ENABLED,
        StudioStatusCode_AWS_STS_REGION_DISABLED,
        StudioStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
        StudioStatusCode_ENCRYPTION_KEY_NOT_FOUND,
        StudioStatusCode_INTERNAL_ERROR,
        StudioStatusCode_ROLE_COULD_NOT_BE_ASSUMED,
        StudioStatusCode_ROLE_NOT_OWNED_BY_STUDIO_OWNER,
        StudioStatusCode_STUDIO_CREATED,
        StudioStatusCode_STUDIO_CREATE_IN_PROGRESS,
        StudioStatusCode_STUDIO_DELETED,
        StudioStatusCode_STUDIO_DELETE_IN_PROGRESS,
        StudioStatusCode_STUDIO_UPDATED,
        StudioStatusCode_STUDIO_UPDATE_IN_PROGRESS,
        StudioStatusCode_STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED,
        StudioStatusCode_STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED,
        StudioStatusCode_STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status code.
newtype StudioStatusCode = StudioStatusCode'
  { fromStudioStatusCode ::
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

pattern StudioStatusCode_AWS_SSO_ACCESS_DENIED :: StudioStatusCode
pattern StudioStatusCode_AWS_SSO_ACCESS_DENIED = StudioStatusCode' "AWS_SSO_ACCESS_DENIED"

pattern StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIRED :: StudioStatusCode
pattern StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIRED = StudioStatusCode' "AWS_SSO_CONFIGURATION_REPAIRED"

pattern StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS :: StudioStatusCode
pattern StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS = StudioStatusCode' "AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS"

pattern StudioStatusCode_AWS_SSO_NOT_ENABLED :: StudioStatusCode
pattern StudioStatusCode_AWS_SSO_NOT_ENABLED = StudioStatusCode' "AWS_SSO_NOT_ENABLED"

pattern StudioStatusCode_AWS_STS_REGION_DISABLED :: StudioStatusCode
pattern StudioStatusCode_AWS_STS_REGION_DISABLED = StudioStatusCode' "AWS_STS_REGION_DISABLED"

pattern StudioStatusCode_ENCRYPTION_KEY_ACCESS_DENIED :: StudioStatusCode
pattern StudioStatusCode_ENCRYPTION_KEY_ACCESS_DENIED = StudioStatusCode' "ENCRYPTION_KEY_ACCESS_DENIED"

pattern StudioStatusCode_ENCRYPTION_KEY_NOT_FOUND :: StudioStatusCode
pattern StudioStatusCode_ENCRYPTION_KEY_NOT_FOUND = StudioStatusCode' "ENCRYPTION_KEY_NOT_FOUND"

pattern StudioStatusCode_INTERNAL_ERROR :: StudioStatusCode
pattern StudioStatusCode_INTERNAL_ERROR = StudioStatusCode' "INTERNAL_ERROR"

pattern StudioStatusCode_ROLE_COULD_NOT_BE_ASSUMED :: StudioStatusCode
pattern StudioStatusCode_ROLE_COULD_NOT_BE_ASSUMED = StudioStatusCode' "ROLE_COULD_NOT_BE_ASSUMED"

pattern StudioStatusCode_ROLE_NOT_OWNED_BY_STUDIO_OWNER :: StudioStatusCode
pattern StudioStatusCode_ROLE_NOT_OWNED_BY_STUDIO_OWNER = StudioStatusCode' "ROLE_NOT_OWNED_BY_STUDIO_OWNER"

pattern StudioStatusCode_STUDIO_CREATED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_CREATED = StudioStatusCode' "STUDIO_CREATED"

pattern StudioStatusCode_STUDIO_CREATE_IN_PROGRESS :: StudioStatusCode
pattern StudioStatusCode_STUDIO_CREATE_IN_PROGRESS = StudioStatusCode' "STUDIO_CREATE_IN_PROGRESS"

pattern StudioStatusCode_STUDIO_DELETED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_DELETED = StudioStatusCode' "STUDIO_DELETED"

pattern StudioStatusCode_STUDIO_DELETE_IN_PROGRESS :: StudioStatusCode
pattern StudioStatusCode_STUDIO_DELETE_IN_PROGRESS = StudioStatusCode' "STUDIO_DELETE_IN_PROGRESS"

pattern StudioStatusCode_STUDIO_UPDATED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_UPDATED = StudioStatusCode' "STUDIO_UPDATED"

pattern StudioStatusCode_STUDIO_UPDATE_IN_PROGRESS :: StudioStatusCode
pattern StudioStatusCode_STUDIO_UPDATE_IN_PROGRESS = StudioStatusCode' "STUDIO_UPDATE_IN_PROGRESS"

pattern StudioStatusCode_STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED = StudioStatusCode' "STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED"

pattern StudioStatusCode_STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED = StudioStatusCode' "STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED"

pattern StudioStatusCode_STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED :: StudioStatusCode
pattern StudioStatusCode_STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED = StudioStatusCode' "STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED"

{-# COMPLETE
  StudioStatusCode_AWS_SSO_ACCESS_DENIED,
  StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIRED,
  StudioStatusCode_AWS_SSO_CONFIGURATION_REPAIR_IN_PROGRESS,
  StudioStatusCode_AWS_SSO_NOT_ENABLED,
  StudioStatusCode_AWS_STS_REGION_DISABLED,
  StudioStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
  StudioStatusCode_ENCRYPTION_KEY_NOT_FOUND,
  StudioStatusCode_INTERNAL_ERROR,
  StudioStatusCode_ROLE_COULD_NOT_BE_ASSUMED,
  StudioStatusCode_ROLE_NOT_OWNED_BY_STUDIO_OWNER,
  StudioStatusCode_STUDIO_CREATED,
  StudioStatusCode_STUDIO_CREATE_IN_PROGRESS,
  StudioStatusCode_STUDIO_DELETED,
  StudioStatusCode_STUDIO_DELETE_IN_PROGRESS,
  StudioStatusCode_STUDIO_UPDATED,
  StudioStatusCode_STUDIO_UPDATE_IN_PROGRESS,
  StudioStatusCode_STUDIO_WITH_LAUNCH_PROFILES_NOT_DELETED,
  StudioStatusCode_STUDIO_WITH_STREAMING_IMAGES_NOT_DELETED,
  StudioStatusCode_STUDIO_WITH_STUDIO_COMPONENTS_NOT_DELETED,
  StudioStatusCode'
  #-}
