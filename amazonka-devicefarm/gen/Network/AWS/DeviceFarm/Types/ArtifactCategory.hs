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

import qualified Network.AWS.Prelude as Prelude

newtype ArtifactCategory = ArtifactCategory'
  { fromArtifactCategory ::
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
