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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- To exclude the DDS from this set of captions: Keep the default, None. To
-- include the DDS: Choose Specified. When you do, also specify the offset
-- coordinates of the display window with DDS x-coordinate and DDS
-- y-coordinate. To include the DDS, but not include display window data:
-- Choose No display window. When you do, you can write position metadata
-- to the page composition segment (PCS) with DDS x-coordinate and DDS
-- y-coordinate. For video resolutions with a height of 576 pixels or less,
-- MediaConvert doesn\'t include the DDS, regardless of the value you
-- choose for DDS handling. All burn-in and DVB-Sub font settings must
-- match.
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
