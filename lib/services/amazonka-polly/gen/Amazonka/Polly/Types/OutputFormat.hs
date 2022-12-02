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
-- Module      : Amazonka.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Polly.Types.OutputFormat
  ( OutputFormat
      ( ..,
        OutputFormat_Json,
        OutputFormat_Mp3,
        OutputFormat_Ogg_vorbis,
        OutputFormat_Pcm
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OutputFormat = OutputFormat'
  { fromOutputFormat ::
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

pattern OutputFormat_Json :: OutputFormat
pattern OutputFormat_Json = OutputFormat' "json"

pattern OutputFormat_Mp3 :: OutputFormat
pattern OutputFormat_Mp3 = OutputFormat' "mp3"

pattern OutputFormat_Ogg_vorbis :: OutputFormat
pattern OutputFormat_Ogg_vorbis = OutputFormat' "ogg_vorbis"

pattern OutputFormat_Pcm :: OutputFormat
pattern OutputFormat_Pcm = OutputFormat' "pcm"

{-# COMPLETE
  OutputFormat_Json,
  OutputFormat_Mp3,
  OutputFormat_Ogg_vorbis,
  OutputFormat_Pcm,
  OutputFormat'
  #-}
