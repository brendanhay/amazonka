{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
  ( H265AlternateTransferFunctionSei
      ( H265AlternateTransferFunctionSei',
        HATFSDisabled,
        HATFSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
newtype H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern HATFSDisabled :: H265AlternateTransferFunctionSei
pattern HATFSDisabled = H265AlternateTransferFunctionSei' "DISABLED"

pattern HATFSEnabled :: H265AlternateTransferFunctionSei
pattern HATFSEnabled = H265AlternateTransferFunctionSei' "ENABLED"

{-# COMPLETE
  HATFSDisabled,
  HATFSEnabled,
  H265AlternateTransferFunctionSei'
  #-}
