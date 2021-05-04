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
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioBufferModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAudioBufferModel
  ( M2tsAudioBufferModel
      ( ..,
        M2tsAudioBufferModel_ATSC,
        M2tsAudioBufferModel_DVB
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | M2ts Audio Buffer Model
newtype M2tsAudioBufferModel = M2tsAudioBufferModel'
  { fromM2tsAudioBufferModel ::
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

pattern M2tsAudioBufferModel_ATSC :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_ATSC = M2tsAudioBufferModel' "ATSC"

pattern M2tsAudioBufferModel_DVB :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_DVB = M2tsAudioBufferModel' "DVB"

{-# COMPLETE
  M2tsAudioBufferModel_ATSC,
  M2tsAudioBufferModel_DVB,
  M2tsAudioBufferModel'
  #-}
