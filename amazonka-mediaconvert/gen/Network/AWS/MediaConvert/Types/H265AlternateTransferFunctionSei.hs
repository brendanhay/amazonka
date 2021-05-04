{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
  ( H265AlternateTransferFunctionSei
      ( ..,
        H265AlternateTransferFunctionSei_DISABLED,
        H265AlternateTransferFunctionSei_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid
-- Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
newtype H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei'
  { fromH265AlternateTransferFunctionSei ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern H265AlternateTransferFunctionSei_DISABLED :: H265AlternateTransferFunctionSei
pattern H265AlternateTransferFunctionSei_DISABLED = H265AlternateTransferFunctionSei' "DISABLED"

pattern H265AlternateTransferFunctionSei_ENABLED :: H265AlternateTransferFunctionSei
pattern H265AlternateTransferFunctionSei_ENABLED = H265AlternateTransferFunctionSei' "ENABLED"

{-# COMPLETE
  H265AlternateTransferFunctionSei_DISABLED,
  H265AlternateTransferFunctionSei_ENABLED,
  H265AlternateTransferFunctionSei'
  #-}
