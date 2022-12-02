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
-- Module      : Amazonka.MediaConvert.Types.Vp8FramerateControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Vp8FramerateControl
  ( Vp8FramerateControl
      ( ..,
        Vp8FramerateControl_INITIALIZE_FROM_SOURCE,
        Vp8FramerateControl_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If you are using the console, use the Framerate setting to specify the
-- frame rate for this output. If you want to keep the same frame rate as
-- the input video, choose Follow source. If you want to do frame rate
-- conversion, choose a frame rate from the dropdown list or choose Custom.
-- The framerates shown in the dropdown list are decimal approximations of
-- fractions. If you choose Custom, specify your frame rate as a fraction.
-- If you are creating your transcoding job specification as a JSON file
-- without the console, use FramerateControl to specify which value the
-- service uses for the frame rate for this output. Choose
-- INITIALIZE_FROM_SOURCE if you want the service to use the frame rate
-- from the input. Choose SPECIFIED if you want the service to use the
-- frame rate you specify in the settings FramerateNumerator and
-- FramerateDenominator.
newtype Vp8FramerateControl = Vp8FramerateControl'
  { fromVp8FramerateControl ::
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

pattern Vp8FramerateControl_INITIALIZE_FROM_SOURCE :: Vp8FramerateControl
pattern Vp8FramerateControl_INITIALIZE_FROM_SOURCE = Vp8FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern Vp8FramerateControl_SPECIFIED :: Vp8FramerateControl
pattern Vp8FramerateControl_SPECIFIED = Vp8FramerateControl' "SPECIFIED"

{-# COMPLETE
  Vp8FramerateControl_INITIALIZE_FROM_SOURCE,
  Vp8FramerateControl_SPECIFIED,
  Vp8FramerateControl'
  #-}
