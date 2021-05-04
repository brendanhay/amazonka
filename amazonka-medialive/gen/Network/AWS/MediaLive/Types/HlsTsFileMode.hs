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
-- Module      : Network.AWS.MediaLive.Types.HlsTsFileMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTsFileMode
  ( HlsTsFileMode
      ( ..,
        HlsTsFileMode_SEGMENTED_FILES,
        HlsTsFileMode_SINGLE_FILE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Ts File Mode
newtype HlsTsFileMode = HlsTsFileMode'
  { fromHlsTsFileMode ::
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

pattern HlsTsFileMode_SEGMENTED_FILES :: HlsTsFileMode
pattern HlsTsFileMode_SEGMENTED_FILES = HlsTsFileMode' "SEGMENTED_FILES"

pattern HlsTsFileMode_SINGLE_FILE :: HlsTsFileMode
pattern HlsTsFileMode_SINGLE_FILE = HlsTsFileMode' "SINGLE_FILE"

{-# COMPLETE
  HlsTsFileMode_SEGMENTED_FILES,
  HlsTsFileMode_SINGLE_FILE,
  HlsTsFileMode'
  #-}
