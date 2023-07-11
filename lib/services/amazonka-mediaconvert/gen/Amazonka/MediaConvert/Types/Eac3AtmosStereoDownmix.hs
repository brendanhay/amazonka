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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosStereoDownmix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosStereoDownmix
  ( Eac3AtmosStereoDownmix
      ( ..,
        Eac3AtmosStereoDownmix_DPL2,
        Eac3AtmosStereoDownmix_NOT_INDICATED,
        Eac3AtmosStereoDownmix_STEREO,
        Eac3AtmosStereoDownmix_SURROUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose how the service does stereo downmixing. Default value: Not
-- indicated (ATMOS_STORAGE_DDP_DMIXMOD_NOT_INDICATED) Related setting: To
-- have MediaConvert use this value, keep the default value, Custom
-- (SPECIFIED) for the setting Downmix control (DownmixControl). Otherwise,
-- MediaConvert ignores Stereo downmix (StereoDownmix).
newtype Eac3AtmosStereoDownmix = Eac3AtmosStereoDownmix'
  { fromEac3AtmosStereoDownmix ::
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

pattern Eac3AtmosStereoDownmix_DPL2 :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmix_DPL2 = Eac3AtmosStereoDownmix' "DPL2"

pattern Eac3AtmosStereoDownmix_NOT_INDICATED :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmix_NOT_INDICATED = Eac3AtmosStereoDownmix' "NOT_INDICATED"

pattern Eac3AtmosStereoDownmix_STEREO :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmix_STEREO = Eac3AtmosStereoDownmix' "STEREO"

pattern Eac3AtmosStereoDownmix_SURROUND :: Eac3AtmosStereoDownmix
pattern Eac3AtmosStereoDownmix_SURROUND = Eac3AtmosStereoDownmix' "SURROUND"

{-# COMPLETE
  Eac3AtmosStereoDownmix_DPL2,
  Eac3AtmosStereoDownmix_NOT_INDICATED,
  Eac3AtmosStereoDownmix_STEREO,
  Eac3AtmosStereoDownmix_SURROUND,
  Eac3AtmosStereoDownmix'
  #-}
