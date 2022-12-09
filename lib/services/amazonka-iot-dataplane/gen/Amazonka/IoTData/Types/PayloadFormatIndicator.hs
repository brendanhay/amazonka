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
-- Module      : Amazonka.IoTData.Types.PayloadFormatIndicator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTData.Types.PayloadFormatIndicator
  ( PayloadFormatIndicator
      ( ..,
        PayloadFormatIndicator_UNSPECIFIED_BYTES,
        PayloadFormatIndicator_UTF8_DATA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PayloadFormatIndicator = PayloadFormatIndicator'
  { fromPayloadFormatIndicator ::
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

pattern PayloadFormatIndicator_UNSPECIFIED_BYTES :: PayloadFormatIndicator
pattern PayloadFormatIndicator_UNSPECIFIED_BYTES = PayloadFormatIndicator' "UNSPECIFIED_BYTES"

pattern PayloadFormatIndicator_UTF8_DATA :: PayloadFormatIndicator
pattern PayloadFormatIndicator_UTF8_DATA = PayloadFormatIndicator' "UTF8_DATA"

{-# COMPLETE
  PayloadFormatIndicator_UNSPECIFIED_BYTES,
  PayloadFormatIndicator_UTF8_DATA,
  PayloadFormatIndicator'
  #-}
