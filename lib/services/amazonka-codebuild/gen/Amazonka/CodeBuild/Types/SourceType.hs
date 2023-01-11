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
-- Module      : Amazonka.CodeBuild.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.SourceType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
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
