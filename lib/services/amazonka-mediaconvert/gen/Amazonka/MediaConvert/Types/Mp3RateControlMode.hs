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
-- Module      : Amazonka.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Mp3RateControlMode
  ( Mp3RateControlMode
      ( ..,
        Mp3RateControlMode_CBR,
        Mp3RateControlMode_VBR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether the service encodes this MP3 audio output with a
-- constant bitrate (CBR) or a variable bitrate (VBR).
newtype Mp3RateControlMode = Mp3RateControlMode'
  { fromMp3RateControlMode ::
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

pattern Mp3RateControlMode_CBR :: Mp3RateControlMode
pattern Mp3RateControlMode_CBR = Mp3RateControlMode' "CBR"

pattern Mp3RateControlMode_VBR :: Mp3RateControlMode
pattern Mp3RateControlMode_VBR = Mp3RateControlMode' "VBR"

{-# COMPLETE
  Mp3RateControlMode_CBR,
  Mp3RateControlMode_VBR,
  Mp3RateControlMode'
  #-}
