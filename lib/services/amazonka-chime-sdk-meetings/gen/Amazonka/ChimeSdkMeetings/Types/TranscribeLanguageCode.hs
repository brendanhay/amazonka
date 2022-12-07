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
-- Module      : Amazonka.ChimeSdkMeetings.Types.TranscribeLanguageCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.TranscribeLanguageCode
  ( TranscribeLanguageCode
      ( ..,
        TranscribeLanguageCode_De_DE,
        TranscribeLanguageCode_En_AU,
        TranscribeLanguageCode_En_GB,
        TranscribeLanguageCode_En_US,
        TranscribeLanguageCode_Es_US,
        TranscribeLanguageCode_Fr_CA,
        TranscribeLanguageCode_Fr_FR,
        TranscribeLanguageCode_It_IT,
        TranscribeLanguageCode_Ja_JP,
        TranscribeLanguageCode_Ko_KR,
        TranscribeLanguageCode_Pt_BR,
        TranscribeLanguageCode_Zh_CN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribeLanguageCode = TranscribeLanguageCode'
  { fromTranscribeLanguageCode ::
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

pattern TranscribeLanguageCode_De_DE :: TranscribeLanguageCode
pattern TranscribeLanguageCode_De_DE = TranscribeLanguageCode' "de-DE"

pattern TranscribeLanguageCode_En_AU :: TranscribeLanguageCode
pattern TranscribeLanguageCode_En_AU = TranscribeLanguageCode' "en-AU"

pattern TranscribeLanguageCode_En_GB :: TranscribeLanguageCode
pattern TranscribeLanguageCode_En_GB = TranscribeLanguageCode' "en-GB"

pattern TranscribeLanguageCode_En_US :: TranscribeLanguageCode
pattern TranscribeLanguageCode_En_US = TranscribeLanguageCode' "en-US"

pattern TranscribeLanguageCode_Es_US :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Es_US = TranscribeLanguageCode' "es-US"

pattern TranscribeLanguageCode_Fr_CA :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Fr_CA = TranscribeLanguageCode' "fr-CA"

pattern TranscribeLanguageCode_Fr_FR :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Fr_FR = TranscribeLanguageCode' "fr-FR"

pattern TranscribeLanguageCode_It_IT :: TranscribeLanguageCode
pattern TranscribeLanguageCode_It_IT = TranscribeLanguageCode' "it-IT"

pattern TranscribeLanguageCode_Ja_JP :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Ja_JP = TranscribeLanguageCode' "ja-JP"

pattern TranscribeLanguageCode_Ko_KR :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Ko_KR = TranscribeLanguageCode' "ko-KR"

pattern TranscribeLanguageCode_Pt_BR :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Pt_BR = TranscribeLanguageCode' "pt-BR"

pattern TranscribeLanguageCode_Zh_CN :: TranscribeLanguageCode
pattern TranscribeLanguageCode_Zh_CN = TranscribeLanguageCode' "zh-CN"

{-# COMPLETE
  TranscribeLanguageCode_De_DE,
  TranscribeLanguageCode_En_AU,
  TranscribeLanguageCode_En_GB,
  TranscribeLanguageCode_En_US,
  TranscribeLanguageCode_Es_US,
  TranscribeLanguageCode_Fr_CA,
  TranscribeLanguageCode_Fr_FR,
  TranscribeLanguageCode_It_IT,
  TranscribeLanguageCode_Ja_JP,
  TranscribeLanguageCode_Ko_KR,
  TranscribeLanguageCode_Pt_BR,
  TranscribeLanguageCode_Zh_CN,
  TranscribeLanguageCode'
  #-}
