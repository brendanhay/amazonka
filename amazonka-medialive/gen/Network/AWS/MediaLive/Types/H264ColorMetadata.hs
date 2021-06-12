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
-- Module      : Network.AWS.MediaLive.Types.H264ColorMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ColorMetadata
  ( H264ColorMetadata
      ( ..,
        H264ColorMetadata_IGNORE,
        H264ColorMetadata_INSERT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Color Metadata
newtype H264ColorMetadata = H264ColorMetadata'
  { fromH264ColorMetadata ::
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

pattern H264ColorMetadata_IGNORE :: H264ColorMetadata
pattern H264ColorMetadata_IGNORE = H264ColorMetadata' "IGNORE"

pattern H264ColorMetadata_INSERT :: H264ColorMetadata
pattern H264ColorMetadata_INSERT = H264ColorMetadata' "INSERT"

{-# COMPLETE
  H264ColorMetadata_IGNORE,
  H264ColorMetadata_INSERT,
  H264ColorMetadata'
  #-}
