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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
  ( ContainerFormat
      ( ..,
        ContainerFormat_FRAGMENTED_MP4,
        ContainerFormat_MPEG_TS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContainerFormat = ContainerFormat'
  { fromContainerFormat ::
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

pattern ContainerFormat_FRAGMENTED_MP4 :: ContainerFormat
pattern ContainerFormat_FRAGMENTED_MP4 = ContainerFormat' "FRAGMENTED_MP4"

pattern ContainerFormat_MPEG_TS :: ContainerFormat
pattern ContainerFormat_MPEG_TS = ContainerFormat' "MPEG_TS"

{-# COMPLETE
  ContainerFormat_FRAGMENTED_MP4,
  ContainerFormat_MPEG_TS,
  ContainerFormat'
  #-}
