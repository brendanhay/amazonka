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

import qualified Network.AWS.Prelude as Prelude

-- | Specify whether your DVB subtitles are standard or for hearing impaired.
-- Choose hearing impaired if your subtitles include audio descriptions and
-- dialogue. Choose standard if your subtitles include only dialogue.
newtype DvbSubtitlingType = DvbSubtitlingType'
  { fromDvbSubtitlingType ::
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

pattern DvbSubtitlingType_HEARING_IMPAIRED :: DvbSubtitlingType
pattern DvbSubtitlingType_HEARING_IMPAIRED = DvbSubtitlingType' "HEARING_IMPAIRED"

pattern DvbSubtitlingType_STANDARD :: DvbSubtitlingType
pattern DvbSubtitlingType_STANDARD = DvbSubtitlingType' "STANDARD"

{-# COMPLETE
  DvbSubtitlingType_HEARING_IMPAIRED,
  DvbSubtitlingType_STANDARD,
  DvbSubtitlingType'
  #-}
