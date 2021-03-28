{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265ParControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265ParControl
  ( H265ParControl
    ( H265ParControl'
    , H265ParControlInitializeFromSource
    , H265ParControlSpecified
    , fromH265ParControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. Specify how the service determines the pixel aspect ratio (PAR) for this output. The default behavior, Follow source (INITIALIZE_FROM_SOURCE), uses the PAR from your input video for your output. To specify a different PAR in the console, choose any value other than Follow source. To specify a different PAR by editing the JSON job specification, choose SPECIFIED. When you choose SPECIFIED for this setting, you must also specify values for the parNumerator and parDenominator settings.
newtype H265ParControl = H265ParControl'{fromH265ParControl ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern H265ParControlInitializeFromSource :: H265ParControl
pattern H265ParControlInitializeFromSource = H265ParControl' "INITIALIZE_FROM_SOURCE"

pattern H265ParControlSpecified :: H265ParControl
pattern H265ParControlSpecified = H265ParControl' "SPECIFIED"

{-# COMPLETE 
  H265ParControlInitializeFromSource,

  H265ParControlSpecified,
  H265ParControl'
  #-}
