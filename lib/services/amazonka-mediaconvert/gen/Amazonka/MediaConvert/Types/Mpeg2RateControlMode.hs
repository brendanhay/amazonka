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
-- Module      : Amazonka.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mpeg2RateControlMode
  ( Mpeg2RateControlMode
      ( ..,
        Mpeg2RateControlMode_CBR,
        Mpeg2RateControlMode_VBR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Rate control mode (Mpeg2RateControlMode) to specify whether the
-- bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode'
  { fromMpeg2RateControlMode ::
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

pattern Mpeg2RateControlMode_CBR :: Mpeg2RateControlMode
pattern Mpeg2RateControlMode_CBR = Mpeg2RateControlMode' "CBR"

pattern Mpeg2RateControlMode_VBR :: Mpeg2RateControlMode
pattern Mpeg2RateControlMode_VBR = Mpeg2RateControlMode' "VBR"

{-# COMPLETE
  Mpeg2RateControlMode_CBR,
  Mpeg2RateControlMode_VBR,
  Mpeg2RateControlMode'
  #-}
