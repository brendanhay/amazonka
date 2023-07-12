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
-- Module      : Amazonka.MediaLive.Types.M2tsAudioBufferModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsAudioBufferModel
  ( M2tsAudioBufferModel
      ( ..,
        M2tsAudioBufferModel_ATSC,
        M2tsAudioBufferModel_DVB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M2ts Audio Buffer Model
newtype M2tsAudioBufferModel = M2tsAudioBufferModel'
  { fromM2tsAudioBufferModel ::
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

pattern M2tsAudioBufferModel_ATSC :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_ATSC = M2tsAudioBufferModel' "ATSC"

pattern M2tsAudioBufferModel_DVB :: M2tsAudioBufferModel
pattern M2tsAudioBufferModel_DVB = M2tsAudioBufferModel' "DVB"

{-# COMPLETE
  M2tsAudioBufferModel_ATSC,
  M2tsAudioBufferModel_DVB,
  M2tsAudioBufferModel'
  #-}
