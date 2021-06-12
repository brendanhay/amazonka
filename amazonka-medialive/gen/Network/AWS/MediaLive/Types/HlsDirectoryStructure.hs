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
-- Module      : Network.AWS.MediaLive.Types.HlsDirectoryStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsDirectoryStructure
  ( HlsDirectoryStructure
      ( ..,
        HlsDirectoryStructure_SINGLE_DIRECTORY,
        HlsDirectoryStructure_SUBDIRECTORY_PER_STREAM
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Directory Structure
newtype HlsDirectoryStructure = HlsDirectoryStructure'
  { fromHlsDirectoryStructure ::
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

pattern HlsDirectoryStructure_SINGLE_DIRECTORY :: HlsDirectoryStructure
pattern HlsDirectoryStructure_SINGLE_DIRECTORY = HlsDirectoryStructure' "SINGLE_DIRECTORY"

pattern HlsDirectoryStructure_SUBDIRECTORY_PER_STREAM :: HlsDirectoryStructure
pattern HlsDirectoryStructure_SUBDIRECTORY_PER_STREAM = HlsDirectoryStructure' "SUBDIRECTORY_PER_STREAM"

{-# COMPLETE
  HlsDirectoryStructure_SINGLE_DIRECTORY,
  HlsDirectoryStructure_SUBDIRECTORY_PER_STREAM,
  HlsDirectoryStructure'
  #-}
