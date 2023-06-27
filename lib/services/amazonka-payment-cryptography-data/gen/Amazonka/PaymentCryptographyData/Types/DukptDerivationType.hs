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
-- Module      : Amazonka.PaymentCryptographyData.Types.DukptDerivationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DukptDerivationType
  ( DukptDerivationType
      ( ..,
        DukptDerivationType_AES_128,
        DukptDerivationType_AES_192,
        DukptDerivationType_AES_256,
        DukptDerivationType_TDES_2KEY,
        DukptDerivationType_TDES_3KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DukptDerivationType = DukptDerivationType'
  { fromDukptDerivationType ::
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

pattern DukptDerivationType_AES_128 :: DukptDerivationType
pattern DukptDerivationType_AES_128 = DukptDerivationType' "AES_128"

pattern DukptDerivationType_AES_192 :: DukptDerivationType
pattern DukptDerivationType_AES_192 = DukptDerivationType' "AES_192"

pattern DukptDerivationType_AES_256 :: DukptDerivationType
pattern DukptDerivationType_AES_256 = DukptDerivationType' "AES_256"

pattern DukptDerivationType_TDES_2KEY :: DukptDerivationType
pattern DukptDerivationType_TDES_2KEY = DukptDerivationType' "TDES_2KEY"

pattern DukptDerivationType_TDES_3KEY :: DukptDerivationType
pattern DukptDerivationType_TDES_3KEY = DukptDerivationType' "TDES_3KEY"

{-# COMPLETE
  DukptDerivationType_AES_128,
  DukptDerivationType_AES_192,
  DukptDerivationType_AES_256,
  DukptDerivationType_TDES_2KEY,
  DukptDerivationType_TDES_3KEY,
  DukptDerivationType'
  #-}
