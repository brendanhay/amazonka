{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceType
  ( SourceType
      ( SourceType',
        STCodecommit,
        STCodepipeline,
        STGithub,
        STS3,
        STBitbucket,
        STGithubEnterprise,
        STNoSource
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SourceType = SourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern STCodecommit :: SourceType
pattern STCodecommit = SourceType' "CODECOMMIT"

pattern STCodepipeline :: SourceType
pattern STCodepipeline = SourceType' "CODEPIPELINE"

pattern STGithub :: SourceType
pattern STGithub = SourceType' "GITHUB"

pattern STS3 :: SourceType
pattern STS3 = SourceType' "S3"

pattern STBitbucket :: SourceType
pattern STBitbucket = SourceType' "BITBUCKET"

pattern STGithubEnterprise :: SourceType
pattern STGithubEnterprise = SourceType' "GITHUB_ENTERPRISE"

pattern STNoSource :: SourceType
pattern STNoSource = SourceType' "NO_SOURCE"

{-# COMPLETE
  STCodecommit,
  STCodepipeline,
  STGithub,
  STS3,
  STBitbucket,
  STGithubEnterprise,
  STNoSource,
  SourceType'
  #-}
