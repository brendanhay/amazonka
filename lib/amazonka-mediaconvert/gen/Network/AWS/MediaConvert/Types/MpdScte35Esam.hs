{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Esam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Esam
  ( MpdScte35Esam
      ( MpdScte35Esam',
        MpdScte35EsamInsert,
        MpdScte35EsamNone,
        fromMpdScte35Esam
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
newtype MpdScte35Esam = MpdScte35Esam'
  { fromMpdScte35Esam ::
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

pattern MpdScte35EsamInsert :: MpdScte35Esam
pattern MpdScte35EsamInsert = MpdScte35Esam' "INSERT"

pattern MpdScte35EsamNone :: MpdScte35Esam
pattern MpdScte35EsamNone = MpdScte35Esam' "NONE"

{-# COMPLETE
  MpdScte35EsamInsert,
  MpdScte35EsamNone,
  MpdScte35Esam'
  #-}
