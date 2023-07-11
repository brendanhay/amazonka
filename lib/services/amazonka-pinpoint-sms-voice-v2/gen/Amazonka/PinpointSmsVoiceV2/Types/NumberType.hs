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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.NumberType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.NumberType
  ( NumberType
      ( ..,
        NumberType_LONG_CODE,
        NumberType_SHORT_CODE,
        NumberType_TEN_DLC,
        NumberType_TOLL_FREE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NumberType = NumberType'
  { fromNumberType ::
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

pattern NumberType_LONG_CODE :: NumberType
pattern NumberType_LONG_CODE = NumberType' "LONG_CODE"

pattern NumberType_SHORT_CODE :: NumberType
pattern NumberType_SHORT_CODE = NumberType' "SHORT_CODE"

pattern NumberType_TEN_DLC :: NumberType
pattern NumberType_TEN_DLC = NumberType' "TEN_DLC"

pattern NumberType_TOLL_FREE :: NumberType
pattern NumberType_TOLL_FREE = NumberType' "TOLL_FREE"

{-# COMPLETE
  NumberType_LONG_CODE,
  NumberType_SHORT_CODE,
  NumberType_TEN_DLC,
  NumberType_TOLL_FREE,
  NumberType'
  #-}
