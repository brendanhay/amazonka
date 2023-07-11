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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.RequestableNumberType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.RequestableNumberType
  ( RequestableNumberType
      ( ..,
        RequestableNumberType_LONG_CODE,
        RequestableNumberType_TEN_DLC,
        RequestableNumberType_TOLL_FREE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RequestableNumberType = RequestableNumberType'
  { fromRequestableNumberType ::
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

pattern RequestableNumberType_LONG_CODE :: RequestableNumberType
pattern RequestableNumberType_LONG_CODE = RequestableNumberType' "LONG_CODE"

pattern RequestableNumberType_TEN_DLC :: RequestableNumberType
pattern RequestableNumberType_TEN_DLC = RequestableNumberType' "TEN_DLC"

pattern RequestableNumberType_TOLL_FREE :: RequestableNumberType
pattern RequestableNumberType_TOLL_FREE = RequestableNumberType' "TOLL_FREE"

{-# COMPLETE
  RequestableNumberType_LONG_CODE,
  RequestableNumberType_TEN_DLC,
  RequestableNumberType_TOLL_FREE,
  RequestableNumberType'
  #-}
