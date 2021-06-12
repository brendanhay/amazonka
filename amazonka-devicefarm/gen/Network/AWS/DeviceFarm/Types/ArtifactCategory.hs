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
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ArtifactCategory
  ( ArtifactCategory
      ( ..,
        ArtifactCategory_FILE,
        ArtifactCategory_LOG,
        ArtifactCategory_SCREENSHOT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ArtifactCategory = ArtifactCategory'
  { fromArtifactCategory ::
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

pattern ArtifactCategory_FILE :: ArtifactCategory
pattern ArtifactCategory_FILE = ArtifactCategory' "FILE"

pattern ArtifactCategory_LOG :: ArtifactCategory
pattern ArtifactCategory_LOG = ArtifactCategory' "LOG"

pattern ArtifactCategory_SCREENSHOT :: ArtifactCategory
pattern ArtifactCategory_SCREENSHOT = ArtifactCategory' "SCREENSHOT"

{-# COMPLETE
  ArtifactCategory_FILE,
  ArtifactCategory_LOG,
  ArtifactCategory_SCREENSHOT,
  ArtifactCategory'
  #-}
