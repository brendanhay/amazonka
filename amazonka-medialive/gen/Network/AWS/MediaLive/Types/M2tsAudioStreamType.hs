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
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioStreamType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAudioStreamType
  ( M2tsAudioStreamType
      ( ..,
        M2tsAudioStreamType_ATSC,
        M2tsAudioStreamType_DVB
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | M2ts Audio Stream Type
newtype M2tsAudioStreamType = M2tsAudioStreamType'
  { fromM2tsAudioStreamType ::
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

pattern M2tsAudioStreamType_ATSC :: M2tsAudioStreamType
pattern M2tsAudioStreamType_ATSC = M2tsAudioStreamType' "ATSC"

pattern M2tsAudioStreamType_DVB :: M2tsAudioStreamType
pattern M2tsAudioStreamType_DVB = M2tsAudioStreamType' "DVB"

{-# COMPLETE
  M2tsAudioStreamType_ATSC,
  M2tsAudioStreamType_DVB,
  M2tsAudioStreamType'
  #-}
