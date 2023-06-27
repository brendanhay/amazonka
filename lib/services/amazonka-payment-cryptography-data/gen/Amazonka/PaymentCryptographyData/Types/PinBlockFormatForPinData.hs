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
-- Module      : Amazonka.PaymentCryptographyData.Types.PinBlockFormatForPinData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.PinBlockFormatForPinData
  ( PinBlockFormatForPinData
      ( ..,
        PinBlockFormatForPinData_ISO_FORMAT_0,
        PinBlockFormatForPinData_ISO_FORMAT_3
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PinBlockFormatForPinData = PinBlockFormatForPinData'
  { fromPinBlockFormatForPinData ::
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

pattern PinBlockFormatForPinData_ISO_FORMAT_0 :: PinBlockFormatForPinData
pattern PinBlockFormatForPinData_ISO_FORMAT_0 = PinBlockFormatForPinData' "ISO_FORMAT_0"

pattern PinBlockFormatForPinData_ISO_FORMAT_3 :: PinBlockFormatForPinData
pattern PinBlockFormatForPinData_ISO_FORMAT_3 = PinBlockFormatForPinData' "ISO_FORMAT_3"

{-# COMPLETE
  PinBlockFormatForPinData_ISO_FORMAT_0,
  PinBlockFormatForPinData_ISO_FORMAT_3,
  PinBlockFormatForPinData'
  #-}
