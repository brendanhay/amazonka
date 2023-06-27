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
-- Module      : Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.DukptKeyVariant
  ( DukptKeyVariant
      ( ..,
        DukptKeyVariant_BIDIRECTIONAL,
        DukptKeyVariant_REQUEST,
        DukptKeyVariant_RESPONSE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DukptKeyVariant = DukptKeyVariant'
  { fromDukptKeyVariant ::
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

pattern DukptKeyVariant_BIDIRECTIONAL :: DukptKeyVariant
pattern DukptKeyVariant_BIDIRECTIONAL = DukptKeyVariant' "BIDIRECTIONAL"

pattern DukptKeyVariant_REQUEST :: DukptKeyVariant
pattern DukptKeyVariant_REQUEST = DukptKeyVariant' "REQUEST"

pattern DukptKeyVariant_RESPONSE :: DukptKeyVariant
pattern DukptKeyVariant_RESPONSE = DukptKeyVariant' "RESPONSE"

{-# COMPLETE
  DukptKeyVariant_BIDIRECTIONAL,
  DukptKeyVariant_REQUEST,
  DukptKeyVariant_RESPONSE,
  DukptKeyVariant'
  #-}
