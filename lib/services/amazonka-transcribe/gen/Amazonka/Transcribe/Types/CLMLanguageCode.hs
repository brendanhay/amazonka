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
-- Module      : Amazonka.Transcribe.Types.CLMLanguageCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.CLMLanguageCode
  ( CLMLanguageCode
      ( ..,
        CLMLanguageCode_De_DE,
        CLMLanguageCode_En_AU,
        CLMLanguageCode_En_GB,
        CLMLanguageCode_En_US,
        CLMLanguageCode_Es_US,
        CLMLanguageCode_Hi_IN,
        CLMLanguageCode_Ja_JP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CLMLanguageCode = CLMLanguageCode'
  { fromCLMLanguageCode ::
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

pattern CLMLanguageCode_De_DE :: CLMLanguageCode
pattern CLMLanguageCode_De_DE = CLMLanguageCode' "de-DE"

pattern CLMLanguageCode_En_AU :: CLMLanguageCode
pattern CLMLanguageCode_En_AU = CLMLanguageCode' "en-AU"

pattern CLMLanguageCode_En_GB :: CLMLanguageCode
pattern CLMLanguageCode_En_GB = CLMLanguageCode' "en-GB"

pattern CLMLanguageCode_En_US :: CLMLanguageCode
pattern CLMLanguageCode_En_US = CLMLanguageCode' "en-US"

pattern CLMLanguageCode_Es_US :: CLMLanguageCode
pattern CLMLanguageCode_Es_US = CLMLanguageCode' "es-US"

pattern CLMLanguageCode_Hi_IN :: CLMLanguageCode
pattern CLMLanguageCode_Hi_IN = CLMLanguageCode' "hi-IN"

pattern CLMLanguageCode_Ja_JP :: CLMLanguageCode
pattern CLMLanguageCode_Ja_JP = CLMLanguageCode' "ja-JP"

{-# COMPLETE
  CLMLanguageCode_De_DE,
  CLMLanguageCode_En_AU,
  CLMLanguageCode_En_GB,
  CLMLanguageCode_En_US,
  CLMLanguageCode_Es_US,
  CLMLanguageCode_Hi_IN,
  CLMLanguageCode_Ja_JP,
  CLMLanguageCode'
  #-}
