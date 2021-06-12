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
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Esam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Esam
  ( MpdScte35Esam
      ( ..,
        MpdScte35Esam_INSERT,
        MpdScte35Esam_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
newtype MpdScte35Esam = MpdScte35Esam'
  { fromMpdScte35Esam ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern MpdScte35Esam_INSERT :: MpdScte35Esam
pattern MpdScte35Esam_INSERT = MpdScte35Esam' "INSERT"

pattern MpdScte35Esam_NONE :: MpdScte35Esam
pattern MpdScte35Esam_NONE = MpdScte35Esam' "NONE"

{-# COMPLETE
  MpdScte35Esam_INSERT,
  MpdScte35Esam_NONE,
  MpdScte35Esam'
  #-}
