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
-- Module      : Amazonka.Rekognition.Types.EmotionName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EmotionName
  ( EmotionName
      ( ..,
        EmotionName_ANGRY,
        EmotionName_CALM,
        EmotionName_CONFUSED,
        EmotionName_DISGUSTED,
        EmotionName_FEAR,
        EmotionName_HAPPY,
        EmotionName_SAD,
        EmotionName_SURPRISED,
        EmotionName_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EmotionName = EmotionName'
  { fromEmotionName ::
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

pattern EmotionName_ANGRY :: EmotionName
pattern EmotionName_ANGRY = EmotionName' "ANGRY"

pattern EmotionName_CALM :: EmotionName
pattern EmotionName_CALM = EmotionName' "CALM"

pattern EmotionName_CONFUSED :: EmotionName
pattern EmotionName_CONFUSED = EmotionName' "CONFUSED"

pattern EmotionName_DISGUSTED :: EmotionName
pattern EmotionName_DISGUSTED = EmotionName' "DISGUSTED"

pattern EmotionName_FEAR :: EmotionName
pattern EmotionName_FEAR = EmotionName' "FEAR"

pattern EmotionName_HAPPY :: EmotionName
pattern EmotionName_HAPPY = EmotionName' "HAPPY"

pattern EmotionName_SAD :: EmotionName
pattern EmotionName_SAD = EmotionName' "SAD"

pattern EmotionName_SURPRISED :: EmotionName
pattern EmotionName_SURPRISED = EmotionName' "SURPRISED"

pattern EmotionName_UNKNOWN :: EmotionName
pattern EmotionName_UNKNOWN = EmotionName' "UNKNOWN"

{-# COMPLETE
  EmotionName_ANGRY,
  EmotionName_CALM,
  EmotionName_CONFUSED,
  EmotionName_DISGUSTED,
  EmotionName_FEAR,
  EmotionName_HAPPY,
  EmotionName_SAD,
  EmotionName_SURPRISED,
  EmotionName_UNKNOWN,
  EmotionName'
  #-}
