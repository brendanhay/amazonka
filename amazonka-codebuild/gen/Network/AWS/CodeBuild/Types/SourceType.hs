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
-- Module      : Network.AWS.CodeBuild.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_BITBUCKET,
        SourceType_CODECOMMIT,
        SourceType_CODEPIPELINE,
        SourceType_GITHUB,
        SourceType_GITHUB_ENTERPRISE,
        SourceType_NO_SOURCE,
        SourceType_S3
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SourceType = SourceType'
  { fromSourceType ::
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

pattern SourceType_BITBUCKET :: SourceType
pattern SourceType_BITBUCKET = SourceType' "BITBUCKET"

pattern SourceType_CODECOMMIT :: SourceType
pattern SourceType_CODECOMMIT = SourceType' "CODECOMMIT"

pattern SourceType_CODEPIPELINE :: SourceType
pattern SourceType_CODEPIPELINE = SourceType' "CODEPIPELINE"

pattern SourceType_GITHUB :: SourceType
pattern SourceType_GITHUB = SourceType' "GITHUB"

pattern SourceType_GITHUB_ENTERPRISE :: SourceType
pattern SourceType_GITHUB_ENTERPRISE = SourceType' "GITHUB_ENTERPRISE"

pattern SourceType_NO_SOURCE :: SourceType
pattern SourceType_NO_SOURCE = SourceType' "NO_SOURCE"

pattern SourceType_S3 :: SourceType
pattern SourceType_S3 = SourceType' "S3"

{-# COMPLETE
  SourceType_BITBUCKET,
  SourceType_CODECOMMIT,
  SourceType_CODEPIPELINE,
  SourceType_GITHUB,
  SourceType_GITHUB_ENTERPRISE,
  SourceType_NO_SOURCE,
  SourceType_S3,
  SourceType'
  #-}
