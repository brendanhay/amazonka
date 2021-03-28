{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
  ( H265AlternateTransferFunctionSei
    ( H265AlternateTransferFunctionSei'
    , H265AlternateTransferFunctionSeiDisabled
    , H265AlternateTransferFunctionSeiEnabled
    , fromH265AlternateTransferFunctionSei
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
newtype H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei'{fromH265AlternateTransferFunctionSei
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern H265AlternateTransferFunctionSeiDisabled :: H265AlternateTransferFunctionSei
pattern H265AlternateTransferFunctionSeiDisabled = H265AlternateTransferFunctionSei' "DISABLED"

pattern H265AlternateTransferFunctionSeiEnabled :: H265AlternateTransferFunctionSei
pattern H265AlternateTransferFunctionSeiEnabled = H265AlternateTransferFunctionSei' "ENABLED"

{-# COMPLETE 
  H265AlternateTransferFunctionSeiDisabled,

  H265AlternateTransferFunctionSeiEnabled,
  H265AlternateTransferFunctionSei'
  #-}
