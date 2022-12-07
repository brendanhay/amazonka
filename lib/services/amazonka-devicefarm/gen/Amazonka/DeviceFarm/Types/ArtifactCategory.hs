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
-- Module      : Amazonka.DeviceFarm.Types.ArtifactCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ArtifactCategory
  ( ArtifactCategory
      ( ..,
        ArtifactCategory_FILE,
        ArtifactCategory_LOG,
        ArtifactCategory_SCREENSHOT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArtifactCategory = ArtifactCategory'
  { fromArtifactCategory ::
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
