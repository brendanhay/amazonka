{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcScte35Esam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcScte35Esam
  ( CmfcScte35Esam
      ( CmfcScte35Esam',
        CmfcScte35EsamInsert,
        CmfcScte35EsamNone,
        fromCmfcScte35Esam
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
newtype CmfcScte35Esam = CmfcScte35Esam'
  { fromCmfcScte35Esam ::
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

pattern CmfcScte35EsamInsert :: CmfcScte35Esam
pattern CmfcScte35EsamInsert = CmfcScte35Esam' "INSERT"

pattern CmfcScte35EsamNone :: CmfcScte35Esam
pattern CmfcScte35EsamNone = CmfcScte35Esam' "NONE"

{-# COMPLETE
  CmfcScte35EsamInsert,
  CmfcScte35EsamNone,
  CmfcScte35Esam'
  #-}
