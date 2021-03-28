{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2ParControl
  ( Mpeg2ParControl
    ( Mpeg2ParControl'
    , Mpeg2ParControlInitializeFromSource
    , Mpeg2ParControlSpecified
    , fromMpeg2ParControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
newtype Mpeg2ParControl = Mpeg2ParControl'{fromMpeg2ParControl ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern Mpeg2ParControlInitializeFromSource :: Mpeg2ParControl
pattern Mpeg2ParControlInitializeFromSource = Mpeg2ParControl' "INITIALIZE_FROM_SOURCE"

pattern Mpeg2ParControlSpecified :: Mpeg2ParControl
pattern Mpeg2ParControlSpecified = Mpeg2ParControl' "SPECIFIED"

{-# COMPLETE 
  Mpeg2ParControlInitializeFromSource,

  Mpeg2ParControlSpecified,
  Mpeg2ParControl'
  #-}
