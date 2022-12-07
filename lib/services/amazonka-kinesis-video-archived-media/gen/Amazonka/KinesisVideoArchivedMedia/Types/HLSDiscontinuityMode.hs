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
-- Module      : Amazonka.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
  ( HLSDiscontinuityMode
      ( ..,
        HLSDiscontinuityMode_ALWAYS,
        HLSDiscontinuityMode_NEVER,
        HLSDiscontinuityMode_ON_DISCONTINUITY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HLSDiscontinuityMode = HLSDiscontinuityMode'
  { fromHLSDiscontinuityMode ::
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

pattern HLSDiscontinuityMode_ALWAYS :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_ALWAYS = HLSDiscontinuityMode' "ALWAYS"

pattern HLSDiscontinuityMode_NEVER :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_NEVER = HLSDiscontinuityMode' "NEVER"

pattern HLSDiscontinuityMode_ON_DISCONTINUITY :: HLSDiscontinuityMode
pattern HLSDiscontinuityMode_ON_DISCONTINUITY = HLSDiscontinuityMode' "ON_DISCONTINUITY"

{-# COMPLETE
  HLSDiscontinuityMode_ALWAYS,
  HLSDiscontinuityMode_NEVER,
  HLSDiscontinuityMode_ON_DISCONTINUITY,
  HLSDiscontinuityMode'
  #-}
