{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossImageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossImageType
  ( InputLossImageType
    ( InputLossImageType'
    , InputLossImageTypeColor
    , InputLossImageTypeSlate
    , fromInputLossImageType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Input Loss Image Type
newtype InputLossImageType = InputLossImageType'{fromInputLossImageType
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern InputLossImageTypeColor :: InputLossImageType
pattern InputLossImageTypeColor = InputLossImageType' "COLOR"

pattern InputLossImageTypeSlate :: InputLossImageType
pattern InputLossImageTypeSlate = InputLossImageType' "SLATE"

{-# COMPLETE 
  InputLossImageTypeColor,

  InputLossImageTypeSlate,
  InputLossImageType'
  #-}
