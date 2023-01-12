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
-- Module      : Amazonka.MediaLive.Types.Eac3StereoDownmix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3StereoDownmix
  ( Eac3StereoDownmix
      ( ..,
        Eac3StereoDownmix_DPL2,
        Eac3StereoDownmix_LO_RO,
        Eac3StereoDownmix_LT_RT,
        Eac3StereoDownmix_NOT_INDICATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Stereo Downmix
newtype Eac3StereoDownmix = Eac3StereoDownmix'
  { fromEac3StereoDownmix ::
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

pattern Eac3StereoDownmix_DPL2 :: Eac3StereoDownmix
pattern Eac3StereoDownmix_DPL2 = Eac3StereoDownmix' "DPL2"

pattern Eac3StereoDownmix_LO_RO :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LO_RO = Eac3StereoDownmix' "LO_RO"

pattern Eac3StereoDownmix_LT_RT :: Eac3StereoDownmix
pattern Eac3StereoDownmix_LT_RT = Eac3StereoDownmix' "LT_RT"

pattern Eac3StereoDownmix_NOT_INDICATED :: Eac3StereoDownmix
pattern Eac3StereoDownmix_NOT_INDICATED = Eac3StereoDownmix' "NOT_INDICATED"

{-# COMPLETE
  Eac3StereoDownmix_DPL2,
  Eac3StereoDownmix_LO_RO,
  Eac3StereoDownmix_LT_RT,
  Eac3StereoDownmix_NOT_INDICATED,
  Eac3StereoDownmix'
  #-}
