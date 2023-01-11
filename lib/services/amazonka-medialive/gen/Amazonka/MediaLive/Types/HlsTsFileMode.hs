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
-- Module      : Amazonka.MediaLive.Types.HlsTsFileMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsTsFileMode
  ( HlsTsFileMode
      ( ..,
        HlsTsFileMode_SEGMENTED_FILES,
        HlsTsFileMode_SINGLE_FILE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Ts File Mode
newtype HlsTsFileMode = HlsTsFileMode'
  { fromHlsTsFileMode ::
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

pattern HlsTsFileMode_SEGMENTED_FILES :: HlsTsFileMode
pattern HlsTsFileMode_SEGMENTED_FILES = HlsTsFileMode' "SEGMENTED_FILES"

pattern HlsTsFileMode_SINGLE_FILE :: HlsTsFileMode
pattern HlsTsFileMode_SINGLE_FILE = HlsTsFileMode' "SINGLE_FILE"

{-# COMPLETE
  HlsTsFileMode_SEGMENTED_FILES,
  HlsTsFileMode_SINGLE_FILE,
  HlsTsFileMode'
  #-}
