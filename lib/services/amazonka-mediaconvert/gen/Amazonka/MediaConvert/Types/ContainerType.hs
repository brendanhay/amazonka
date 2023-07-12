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
-- Module      : Amazonka.MediaConvert.Types.ContainerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ContainerType
  ( ContainerType
      ( ..,
        ContainerType_CMFC,
        ContainerType_F4V,
        ContainerType_ISMV,
        ContainerType_M2TS,
        ContainerType_M3U8,
        ContainerType_MOV,
        ContainerType_MP4,
        ContainerType_MPD,
        ContainerType_MXF,
        ContainerType_RAW,
        ContainerType_WEBM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
newtype ContainerType = ContainerType'
  { fromContainerType ::
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

pattern ContainerType_CMFC :: ContainerType
pattern ContainerType_CMFC = ContainerType' "CMFC"

pattern ContainerType_F4V :: ContainerType
pattern ContainerType_F4V = ContainerType' "F4V"

pattern ContainerType_ISMV :: ContainerType
pattern ContainerType_ISMV = ContainerType' "ISMV"

pattern ContainerType_M2TS :: ContainerType
pattern ContainerType_M2TS = ContainerType' "M2TS"

pattern ContainerType_M3U8 :: ContainerType
pattern ContainerType_M3U8 = ContainerType' "M3U8"

pattern ContainerType_MOV :: ContainerType
pattern ContainerType_MOV = ContainerType' "MOV"

pattern ContainerType_MP4 :: ContainerType
pattern ContainerType_MP4 = ContainerType' "MP4"

pattern ContainerType_MPD :: ContainerType
pattern ContainerType_MPD = ContainerType' "MPD"

pattern ContainerType_MXF :: ContainerType
pattern ContainerType_MXF = ContainerType' "MXF"

pattern ContainerType_RAW :: ContainerType
pattern ContainerType_RAW = ContainerType' "RAW"

pattern ContainerType_WEBM :: ContainerType
pattern ContainerType_WEBM = ContainerType' "WEBM"

{-# COMPLETE
  ContainerType_CMFC,
  ContainerType_F4V,
  ContainerType_ISMV,
  ContainerType_M2TS,
  ContainerType_M3U8,
  ContainerType_MOV,
  ContainerType_MP4,
  ContainerType_MPD,
  ContainerType_MXF,
  ContainerType_RAW,
  ContainerType_WEBM,
  ContainerType'
  #-}
