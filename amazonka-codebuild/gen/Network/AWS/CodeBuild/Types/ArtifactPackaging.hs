{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactPackaging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactPackaging
  ( ArtifactPackaging
      ( ..,
        ArtifactPackaging_NONE,
        ArtifactPackaging_ZIP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ArtifactPackaging = ArtifactPackaging'
  { fromArtifactPackaging ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ArtifactPackaging_NONE :: ArtifactPackaging
pattern ArtifactPackaging_NONE = ArtifactPackaging' "NONE"

pattern ArtifactPackaging_ZIP :: ArtifactPackaging
pattern ArtifactPackaging_ZIP = ArtifactPackaging' "ZIP"

{-# COMPLETE
  ArtifactPackaging_NONE,
  ArtifactPackaging_ZIP,
  ArtifactPackaging'
  #-}
