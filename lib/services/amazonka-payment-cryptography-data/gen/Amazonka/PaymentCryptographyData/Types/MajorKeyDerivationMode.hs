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
-- Module      : Amazonka.PaymentCryptographyData.Types.MajorKeyDerivationMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.MajorKeyDerivationMode
  ( MajorKeyDerivationMode
      ( ..,
        MajorKeyDerivationMode_EMV_OPTION_A,
        MajorKeyDerivationMode_EMV_OPTION_B
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MajorKeyDerivationMode = MajorKeyDerivationMode'
  { fromMajorKeyDerivationMode ::
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

pattern MajorKeyDerivationMode_EMV_OPTION_A :: MajorKeyDerivationMode
pattern MajorKeyDerivationMode_EMV_OPTION_A = MajorKeyDerivationMode' "EMV_OPTION_A"

pattern MajorKeyDerivationMode_EMV_OPTION_B :: MajorKeyDerivationMode
pattern MajorKeyDerivationMode_EMV_OPTION_B = MajorKeyDerivationMode' "EMV_OPTION_B"

{-# COMPLETE
  MajorKeyDerivationMode_EMV_OPTION_A,
  MajorKeyDerivationMode_EMV_OPTION_B,
  MajorKeyDerivationMode'
  #-}
