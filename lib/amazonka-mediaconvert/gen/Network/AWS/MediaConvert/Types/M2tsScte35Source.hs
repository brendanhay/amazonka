{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsScte35Source
  ( M2tsScte35Source
      ( M2tsScte35Source',
        M2tsScte35SourcePassthrough,
        M2tsScte35SourceNone,
        fromM2tsScte35Source
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want SCTE-35 markers in this output. For SCTE-35 markers from an ESAM XML document-- Choose None (NONE). Also provide the ESAM XML as a string in the setting Signal processing notification XML (sccXml). Also enable ESAM SCTE-35 (include the property scte35Esam).
newtype M2tsScte35Source = M2tsScte35Source'
  { fromM2tsScte35Source ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern M2tsScte35SourcePassthrough :: M2tsScte35Source
pattern M2tsScte35SourcePassthrough = M2tsScte35Source' "PASSTHROUGH"

pattern M2tsScte35SourceNone :: M2tsScte35Source
pattern M2tsScte35SourceNone = M2tsScte35Source' "NONE"

{-# COMPLETE
  M2tsScte35SourcePassthrough,
  M2tsScte35SourceNone,
  M2tsScte35Source'
  #-}
