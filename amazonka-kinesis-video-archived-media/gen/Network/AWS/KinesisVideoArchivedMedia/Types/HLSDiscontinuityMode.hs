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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
  ( HLSDiscontinuityMode
      ( ..,
        HLSDiscontinuityMode_ALWAYS,
        HLSDiscontinuityMode_NEVER,
        HLSDiscontinuityMode_ON_DISCONTINUITY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HLSDiscontinuityMode = HLSDiscontinuityMode'
  { fromHLSDiscontinuityMode ::
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

pattern HLSDiscontinuityMode_ALWAYS :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_ALWAYS = HLSDiscontinuityMode' "ALWAYS"

pattern HLSDiscontinuityMode_NEVER :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_NEVER = HLSDiscontinuityMode' "NEVER"

pattern HLSDiscontinuityMode_ON_DISCONTINUITY :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_ON_DISCONTINUITY = HLSDiscontinuityMode' "ON_DISCONTINUITY"

{-# COMPLETE
  HLSDiscontinuityMode_ALWAYS,
  HLSDiscontinuityMode_NEVER,
  HLSDiscontinuityMode_ON_DISCONTINUITY,
  HLSDiscontinuityMode'
  #-}
