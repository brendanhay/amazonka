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
-- Module      : Amazonka.MediaConvert.Types.XavcFramerateControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcFramerateControl
  ( XavcFramerateControl
      ( ..,
        XavcFramerateControl_INITIALIZE_FROM_SOURCE,
        XavcFramerateControl_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If you are using the console, use the Frame rate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list. The framerates
-- shown in the dropdown list are decimal approximations of fractions. If
-- you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate that you specify in the settings FramerateNumerator and
-- FramerateDenominator.
newtype XavcFramerateControl = XavcFramerateControl'
  { fromXavcFramerateControl ::
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

pattern XavcFramerateControl_INITIALIZE_FROM_SOURCE :: XavcFramerateControl
pattern XavcFramerateControl_INITIALIZE_FROM_SOURCE = XavcFramerateControl' "INITIALIZE_FROM_SOURCE"

pattern XavcFramerateControl_SPECIFIED :: XavcFramerateControl
pattern XavcFramerateControl_SPECIFIED = XavcFramerateControl' "SPECIFIED"

{-# COMPLETE
  XavcFramerateControl_INITIALIZE_FROM_SOURCE,
  XavcFramerateControl_SPECIFIED,
  XavcFramerateControl'
  #-}
