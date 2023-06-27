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
-- Module      : Amazonka.KMS.Types.WrappingKeySpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.WrappingKeySpec
  ( WrappingKeySpec
      ( ..,
        WrappingKeySpec_RSA_2048,
        WrappingKeySpec_RSA_3072,
        WrappingKeySpec_RSA_4096
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WrappingKeySpec = WrappingKeySpec'
  { fromWrappingKeySpec ::
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

pattern WrappingKeySpec_RSA_2048 :: WrappingKeySpec
pattern WrappingKeySpec_RSA_2048 = WrappingKeySpec' "RSA_2048"

pattern WrappingKeySpec_RSA_3072 :: WrappingKeySpec
pattern WrappingKeySpec_RSA_3072 = WrappingKeySpec' "RSA_3072"

pattern WrappingKeySpec_RSA_4096 :: WrappingKeySpec
pattern WrappingKeySpec_RSA_4096 = WrappingKeySpec' "RSA_4096"

{-# COMPLETE
  WrappingKeySpec_RSA_2048,
  WrappingKeySpec_RSA_3072,
  WrappingKeySpec_RSA_4096,
  WrappingKeySpec'
  #-}
