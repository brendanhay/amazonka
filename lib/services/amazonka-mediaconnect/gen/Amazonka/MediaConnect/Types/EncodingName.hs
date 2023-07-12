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
-- Module      : Amazonka.MediaConnect.Types.EncodingName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.EncodingName
  ( EncodingName
      ( ..,
        EncodingName_Jxsv,
        EncodingName_Pcm,
        EncodingName_Raw,
        EncodingName_Smpte291
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncodingName = EncodingName'
  { fromEncodingName ::
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

pattern EncodingName_Jxsv :: EncodingName
pattern EncodingName_Jxsv = EncodingName' "jxsv"

pattern EncodingName_Pcm :: EncodingName
pattern EncodingName_Pcm = EncodingName' "pcm"

pattern EncodingName_Raw :: EncodingName
pattern EncodingName_Raw = EncodingName' "raw"

pattern EncodingName_Smpte291 :: EncodingName
pattern EncodingName_Smpte291 = EncodingName' "smpte291"

{-# COMPLETE
  EncodingName_Jxsv,
  EncodingName_Pcm,
  EncodingName_Raw,
  EncodingName_Smpte291,
  EncodingName'
  #-}
