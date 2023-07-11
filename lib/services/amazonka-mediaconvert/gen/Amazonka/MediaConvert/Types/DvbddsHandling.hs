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
-- Module      : Amazonka.MediaConvert.Types.DvbddsHandling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbddsHandling
  ( DvbddsHandling
      ( ..,
        DvbddsHandling_NONE,
        DvbddsHandling_NO_DISPLAY_WINDOW,
        DvbddsHandling_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how MediaConvert handles the display definition segment (DDS).
-- Keep the default, None (NONE), to exclude the DDS from this set of
-- captions. Choose No display window (NO_DISPLAY_WINDOW) to have
-- MediaConvert include the DDS but not include display window data. In
-- this case, MediaConvert writes that information to the page composition
-- segment (PCS) instead. Choose Specify (SPECIFIED) to have MediaConvert
-- set up the display window based on the values that you specify in
-- related job settings. For video resolutions that are 576 pixels or
-- smaller in height, MediaConvert doesn\'t include the DDS, regardless of
-- the value you choose for DDS handling (ddsHandling). In this case, it
-- doesn\'t write the display window data to the PCS either. Related
-- settings: Use the settings DDS x-coordinate (ddsXCoordinate) and DDS
-- y-coordinate (ddsYCoordinate) to specify the offset between the top left
-- corner of the display window and the top left corner of the video frame.
-- All burn-in and DVB-Sub font settings must match.
newtype DvbddsHandling = DvbddsHandling'
  { fromDvbddsHandling ::
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

pattern DvbddsHandling_NONE :: DvbddsHandling
pattern DvbddsHandling_NONE = DvbddsHandling' "NONE"

pattern DvbddsHandling_NO_DISPLAY_WINDOW :: DvbddsHandling
pattern DvbddsHandling_NO_DISPLAY_WINDOW = DvbddsHandling' "NO_DISPLAY_WINDOW"

pattern DvbddsHandling_SPECIFIED :: DvbddsHandling
pattern DvbddsHandling_SPECIFIED = DvbddsHandling' "SPECIFIED"

{-# COMPLETE
  DvbddsHandling_NONE,
  DvbddsHandling_NO_DISPLAY_WINDOW,
  DvbddsHandling_SPECIFIED,
  DvbddsHandling'
  #-}
