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
-- Module      : Network.AWS.MediaConvert.Types.CmfcScte35Esam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcScte35Esam
  ( CmfcScte35Esam
      ( ..,
        CmfcScte35Esam_INSERT,
        CmfcScte35Esam_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
newtype CmfcScte35Esam = CmfcScte35Esam'
  { fromCmfcScte35Esam ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CmfcScte35Esam_INSERT :: CmfcScte35Esam
pattern CmfcScte35Esam_INSERT = CmfcScte35Esam' "INSERT"

pattern CmfcScte35Esam_NONE :: CmfcScte35Esam
pattern CmfcScte35Esam_NONE = CmfcScte35Esam' "NONE"

{-# COMPLETE
  CmfcScte35Esam_INSERT,
  CmfcScte35Esam_NONE,
  CmfcScte35Esam'
  #-}
