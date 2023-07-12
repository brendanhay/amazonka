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
-- Module      : Amazonka.MediaConvert.Types.MpdScte35Esam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MpdScte35Esam
  ( MpdScte35Esam
      ( ..,
        MpdScte35Esam_INSERT,
        MpdScte35Esam_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
newtype MpdScte35Esam = MpdScte35Esam'
  { fromMpdScte35Esam ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern MpdScte35Esam_INSERT :: MpdScte35Esam
pattern MpdScte35Esam_INSERT = MpdScte35Esam' "INSERT"

pattern MpdScte35Esam_NONE :: MpdScte35Esam
pattern MpdScte35Esam_NONE = MpdScte35Esam' "NONE"

{-# COMPLETE
  MpdScte35Esam_INSERT,
  MpdScte35Esam_NONE,
  MpdScte35Esam'
  #-}
