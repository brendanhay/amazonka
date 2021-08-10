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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitlingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitlingType
  ( DvbSubtitlingType
      ( ..,
        DvbSubtitlingType_HEARING_IMPAIRED,
        DvbSubtitlingType_STANDARD
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
newtype DvbSubtitlingType = DvbSubtitlingType'
  { fromDvbSubtitlingType ::
      Core.Text
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

pattern DvbSubtitlingType_HEARING_IMPAIRED :: DvbSubtitlingType
pattern DvbSubtitlingType_HEARING_IMPAIRED = DvbSubtitlingType' "HEARING_IMPAIRED"

pattern DvbSubtitlingType_STANDARD :: DvbSubtitlingType
pattern DvbSubtitlingType_STANDARD = DvbSubtitlingType' "STANDARD"

{-# COMPLETE
  DvbSubtitlingType_HEARING_IMPAIRED,
  DvbSubtitlingType_STANDARD,
  DvbSubtitlingType'
  #-}
