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
-- Module      : Amazonka.CodeBuild.Types.ArtifactsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ArtifactsType
  ( ArtifactsType
      ( ..,
        ArtifactsType_CODEPIPELINE,
        ArtifactsType_NO_ARTIFACTS,
        ArtifactsType_S3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArtifactsType = ArtifactsType'
  { fromArtifactsType ::
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

pattern ArtifactsType_CODEPIPELINE :: ArtifactsType
pattern ArtifactsType_CODEPIPELINE = ArtifactsType' "CODEPIPELINE"

pattern ArtifactsType_NO_ARTIFACTS :: ArtifactsType
pattern ArtifactsType_NO_ARTIFACTS = ArtifactsType' "NO_ARTIFACTS"

pattern ArtifactsType_S3 :: ArtifactsType
pattern ArtifactsType_S3 = ArtifactsType' "S3"

{-# COMPLETE
  ArtifactsType_CODEPIPELINE,
  ArtifactsType_NO_ARTIFACTS,
  ArtifactsType_S3,
  ArtifactsType'
  #-}
