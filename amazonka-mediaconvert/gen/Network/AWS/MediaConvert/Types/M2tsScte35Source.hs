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
-- Module      : Network.AWS.MediaConvert.Types.M2tsScte35Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsScte35Source
  ( M2tsScte35Source
      ( ..,
        M2tsScte35Source_NONE,
        M2tsScte35Source_PASSTHROUGH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | For SCTE-35 markers from your input-- Choose Passthrough (PASSTHROUGH)
-- if you want SCTE-35 markers that appear in your input to also appear in
-- this output. Choose None (NONE) if you don\'t want SCTE-35 markers in
-- this output. For SCTE-35 markers from an ESAM XML document-- Choose None
-- (NONE). Also provide the ESAM XML as a string in the setting Signal
-- processing notification XML (sccXml). Also enable ESAM SCTE-35 (include
-- the property scte35Esam).
newtype M2tsScte35Source = M2tsScte35Source'
  { fromM2tsScte35Source ::
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

pattern M2tsScte35Source_NONE :: M2tsScte35Source
pattern M2tsScte35Source_NONE = M2tsScte35Source' "NONE"

pattern M2tsScte35Source_PASSTHROUGH :: M2tsScte35Source
pattern M2tsScte35Source_PASSTHROUGH = M2tsScte35Source' "PASSTHROUGH"

{-# COMPLETE
  M2tsScte35Source_NONE,
  M2tsScte35Source_PASSTHROUGH,
  M2tsScte35Source'
  #-}
