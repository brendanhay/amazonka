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
-- Module      : Amazonka.MediaLive.Types.ReservationResolution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationResolution
  ( ReservationResolution
      ( ..,
        ReservationResolution_FHD,
        ReservationResolution_HD,
        ReservationResolution_SD,
        ReservationResolution_UHD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Resolution based on lines of vertical resolution; SD is less than 720
-- lines, HD is 720 to 1080 lines, FHD is 1080 lines, UHD is greater than
-- 1080 lines
newtype ReservationResolution = ReservationResolution'
  { fromReservationResolution ::
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

pattern ReservationResolution_FHD :: ReservationResolution
pattern ReservationResolution_FHD = ReservationResolution' "FHD"

pattern ReservationResolution_HD :: ReservationResolution
pattern ReservationResolution_HD = ReservationResolution' "HD"

pattern ReservationResolution_SD :: ReservationResolution
pattern ReservationResolution_SD = ReservationResolution' "SD"

pattern ReservationResolution_UHD :: ReservationResolution
pattern ReservationResolution_UHD = ReservationResolution' "UHD"

{-# COMPLETE
  ReservationResolution_FHD,
  ReservationResolution_HD,
  ReservationResolution_SD,
  ReservationResolution_UHD,
  ReservationResolution'
  #-}
