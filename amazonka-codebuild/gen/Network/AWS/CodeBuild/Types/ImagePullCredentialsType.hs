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
-- Module      : Network.AWS.CodeBuild.Types.ImagePullCredentialsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ImagePullCredentialsType
  ( ImagePullCredentialsType
      ( ..,
        ImagePullCredentialsType_CODEBUILD,
        ImagePullCredentialsType_SERVICE_ROLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ImagePullCredentialsType = ImagePullCredentialsType'
  { fromImagePullCredentialsType ::
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

pattern ImagePullCredentialsType_CODEBUILD :: ImagePullCredentialsType
pattern ImagePullCredentialsType_CODEBUILD = ImagePullCredentialsType' "CODEBUILD"

pattern ImagePullCredentialsType_SERVICE_ROLE :: ImagePullCredentialsType
pattern ImagePullCredentialsType_SERVICE_ROLE = ImagePullCredentialsType' "SERVICE_ROLE"

{-# COMPLETE
  ImagePullCredentialsType_CODEBUILD,
  ImagePullCredentialsType_SERVICE_ROLE,
  ImagePullCredentialsType'
  #-}
