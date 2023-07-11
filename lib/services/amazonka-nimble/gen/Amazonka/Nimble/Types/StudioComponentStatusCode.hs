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
-- Module      : Amazonka.Nimble.Types.StudioComponentStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentStatusCode
  ( StudioComponentStatusCode
      ( ..,
        StudioComponentStatusCode_ACTIVE_DIRECTORY_ALREADY_EXISTS,
        StudioComponentStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
        StudioComponentStatusCode_ENCRYPTION_KEY_NOT_FOUND,
        StudioComponentStatusCode_INTERNAL_ERROR,
        StudioComponentStatusCode_STUDIO_COMPONENT_CREATED,
        StudioComponentStatusCode_STUDIO_COMPONENT_CREATE_IN_PROGRESS,
        StudioComponentStatusCode_STUDIO_COMPONENT_DELETED,
        StudioComponentStatusCode_STUDIO_COMPONENT_DELETE_IN_PROGRESS,
        StudioComponentStatusCode_STUDIO_COMPONENT_UPDATED,
        StudioComponentStatusCode_STUDIO_COMPONENT_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the studio component resource.
--
-- When the resource is in the @READY@ state, the status code signals what
-- the last mutation made to the resource was.
--
-- When the resource is in a @CREATE_FAILED@, @UPDATE_FAILED@, or
-- @DELETE_FAILED@ state, the status code signals what went wrong and why
-- the mutation failed.
newtype StudioComponentStatusCode = StudioComponentStatusCode'
  { fromStudioComponentStatusCode ::
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

pattern StudioComponentStatusCode_ACTIVE_DIRECTORY_ALREADY_EXISTS :: StudioComponentStatusCode
pattern StudioComponentStatusCode_ACTIVE_DIRECTORY_ALREADY_EXISTS = StudioComponentStatusCode' "ACTIVE_DIRECTORY_ALREADY_EXISTS"

pattern StudioComponentStatusCode_ENCRYPTION_KEY_ACCESS_DENIED :: StudioComponentStatusCode
pattern StudioComponentStatusCode_ENCRYPTION_KEY_ACCESS_DENIED = StudioComponentStatusCode' "ENCRYPTION_KEY_ACCESS_DENIED"

pattern StudioComponentStatusCode_ENCRYPTION_KEY_NOT_FOUND :: StudioComponentStatusCode
pattern StudioComponentStatusCode_ENCRYPTION_KEY_NOT_FOUND = StudioComponentStatusCode' "ENCRYPTION_KEY_NOT_FOUND"

pattern StudioComponentStatusCode_INTERNAL_ERROR :: StudioComponentStatusCode
pattern StudioComponentStatusCode_INTERNAL_ERROR = StudioComponentStatusCode' "INTERNAL_ERROR"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_CREATED :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_CREATED = StudioComponentStatusCode' "STUDIO_COMPONENT_CREATED"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_CREATE_IN_PROGRESS :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_CREATE_IN_PROGRESS = StudioComponentStatusCode' "STUDIO_COMPONENT_CREATE_IN_PROGRESS"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_DELETED :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_DELETED = StudioComponentStatusCode' "STUDIO_COMPONENT_DELETED"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_DELETE_IN_PROGRESS :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_DELETE_IN_PROGRESS = StudioComponentStatusCode' "STUDIO_COMPONENT_DELETE_IN_PROGRESS"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_UPDATED :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_UPDATED = StudioComponentStatusCode' "STUDIO_COMPONENT_UPDATED"

pattern StudioComponentStatusCode_STUDIO_COMPONENT_UPDATE_IN_PROGRESS :: StudioComponentStatusCode
pattern StudioComponentStatusCode_STUDIO_COMPONENT_UPDATE_IN_PROGRESS = StudioComponentStatusCode' "STUDIO_COMPONENT_UPDATE_IN_PROGRESS"

{-# COMPLETE
  StudioComponentStatusCode_ACTIVE_DIRECTORY_ALREADY_EXISTS,
  StudioComponentStatusCode_ENCRYPTION_KEY_ACCESS_DENIED,
  StudioComponentStatusCode_ENCRYPTION_KEY_NOT_FOUND,
  StudioComponentStatusCode_INTERNAL_ERROR,
  StudioComponentStatusCode_STUDIO_COMPONENT_CREATED,
  StudioComponentStatusCode_STUDIO_COMPONENT_CREATE_IN_PROGRESS,
  StudioComponentStatusCode_STUDIO_COMPONENT_DELETED,
  StudioComponentStatusCode_STUDIO_COMPONENT_DELETE_IN_PROGRESS,
  StudioComponentStatusCode_STUDIO_COMPONENT_UPDATED,
  StudioComponentStatusCode_STUDIO_COMPONENT_UPDATE_IN_PROGRESS,
  StudioComponentStatusCode'
  #-}
