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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.CallAnalyticsLanguageCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.CallAnalyticsLanguageCode
  ( CallAnalyticsLanguageCode
      ( ..,
        CallAnalyticsLanguageCode_De_DE,
        CallAnalyticsLanguageCode_En_AU,
        CallAnalyticsLanguageCode_En_GB,
        CallAnalyticsLanguageCode_En_US,
        CallAnalyticsLanguageCode_Es_US,
        CallAnalyticsLanguageCode_Fr_CA,
        CallAnalyticsLanguageCode_Fr_FR,
        CallAnalyticsLanguageCode_It_IT,
        CallAnalyticsLanguageCode_Pt_BR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CallAnalyticsLanguageCode = CallAnalyticsLanguageCode'
  { fromCallAnalyticsLanguageCode ::
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

pattern CallAnalyticsLanguageCode_De_DE :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_De_DE = CallAnalyticsLanguageCode' "de-DE"

pattern CallAnalyticsLanguageCode_En_AU :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_En_AU = CallAnalyticsLanguageCode' "en-AU"

pattern CallAnalyticsLanguageCode_En_GB :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_En_GB = CallAnalyticsLanguageCode' "en-GB"

pattern CallAnalyticsLanguageCode_En_US :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_En_US = CallAnalyticsLanguageCode' "en-US"

pattern CallAnalyticsLanguageCode_Es_US :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_Es_US = CallAnalyticsLanguageCode' "es-US"

pattern CallAnalyticsLanguageCode_Fr_CA :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_Fr_CA = CallAnalyticsLanguageCode' "fr-CA"

pattern CallAnalyticsLanguageCode_Fr_FR :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_Fr_FR = CallAnalyticsLanguageCode' "fr-FR"

pattern CallAnalyticsLanguageCode_It_IT :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_It_IT = CallAnalyticsLanguageCode' "it-IT"

pattern CallAnalyticsLanguageCode_Pt_BR :: CallAnalyticsLanguageCode
pattern CallAnalyticsLanguageCode_Pt_BR = CallAnalyticsLanguageCode' "pt-BR"

{-# COMPLETE
  CallAnalyticsLanguageCode_De_DE,
  CallAnalyticsLanguageCode_En_AU,
  CallAnalyticsLanguageCode_En_GB,
  CallAnalyticsLanguageCode_En_US,
  CallAnalyticsLanguageCode_Es_US,
  CallAnalyticsLanguageCode_Fr_CA,
  CallAnalyticsLanguageCode_Fr_FR,
  CallAnalyticsLanguageCode_It_IT,
  CallAnalyticsLanguageCode_Pt_BR,
  CallAnalyticsLanguageCode'
  #-}
