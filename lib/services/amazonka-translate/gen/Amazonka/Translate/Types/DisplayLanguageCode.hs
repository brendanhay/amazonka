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
-- Module      : Amazonka.Translate.Types.DisplayLanguageCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.DisplayLanguageCode
  ( DisplayLanguageCode
      ( ..,
        DisplayLanguageCode_De,
        DisplayLanguageCode_En,
        DisplayLanguageCode_Es,
        DisplayLanguageCode_Fr,
        DisplayLanguageCode_It,
        DisplayLanguageCode_Ja,
        DisplayLanguageCode_Ko,
        DisplayLanguageCode_Pt,
        DisplayLanguageCode_Zh,
        DisplayLanguageCode_Zh_TW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DisplayLanguageCode = DisplayLanguageCode'
  { fromDisplayLanguageCode ::
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

pattern DisplayLanguageCode_De :: DisplayLanguageCode
pattern DisplayLanguageCode_De = DisplayLanguageCode' "de"

pattern DisplayLanguageCode_En :: DisplayLanguageCode
pattern DisplayLanguageCode_En = DisplayLanguageCode' "en"

pattern DisplayLanguageCode_Es :: DisplayLanguageCode
pattern DisplayLanguageCode_Es = DisplayLanguageCode' "es"

pattern DisplayLanguageCode_Fr :: DisplayLanguageCode
pattern DisplayLanguageCode_Fr = DisplayLanguageCode' "fr"

pattern DisplayLanguageCode_It :: DisplayLanguageCode
pattern DisplayLanguageCode_It = DisplayLanguageCode' "it"

pattern DisplayLanguageCode_Ja :: DisplayLanguageCode
pattern DisplayLanguageCode_Ja = DisplayLanguageCode' "ja"

pattern DisplayLanguageCode_Ko :: DisplayLanguageCode
pattern DisplayLanguageCode_Ko = DisplayLanguageCode' "ko"

pattern DisplayLanguageCode_Pt :: DisplayLanguageCode
pattern DisplayLanguageCode_Pt = DisplayLanguageCode' "pt"

pattern DisplayLanguageCode_Zh :: DisplayLanguageCode
pattern DisplayLanguageCode_Zh = DisplayLanguageCode' "zh"

pattern DisplayLanguageCode_Zh_TW :: DisplayLanguageCode
pattern DisplayLanguageCode_Zh_TW = DisplayLanguageCode' "zh-TW"

{-# COMPLETE
  DisplayLanguageCode_De,
  DisplayLanguageCode_En,
  DisplayLanguageCode_Es,
  DisplayLanguageCode_Fr,
  DisplayLanguageCode_It,
  DisplayLanguageCode_Ja,
  DisplayLanguageCode_Ko,
  DisplayLanguageCode_Pt,
  DisplayLanguageCode_Zh,
  DisplayLanguageCode_Zh_TW,
  DisplayLanguageCode'
  #-}
