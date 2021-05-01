{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AlgorithmSpec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AlgorithmSpec
  ( AlgorithmSpec
      ( ..,
        AlgorithmSpec_RSAES_OAEP_SHA_1,
        AlgorithmSpec_RSAES_OAEP_SHA_256,
        AlgorithmSpec_RSAES_PKCS1_V1_5
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AlgorithmSpec = AlgorithmSpec'
  { fromAlgorithmSpec ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AlgorithmSpec_RSAES_OAEP_SHA_1 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_OAEP_SHA_1 = AlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern AlgorithmSpec_RSAES_OAEP_SHA_256 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_OAEP_SHA_256 = AlgorithmSpec' "RSAES_OAEP_SHA_256"

pattern AlgorithmSpec_RSAES_PKCS1_V1_5 :: AlgorithmSpec
pattern AlgorithmSpec_RSAES_PKCS1_V1_5 = AlgorithmSpec' "RSAES_PKCS1_V1_5"

{-# COMPLETE
  AlgorithmSpec_RSAES_OAEP_SHA_1,
  AlgorithmSpec_RSAES_OAEP_SHA_256,
  AlgorithmSpec_RSAES_PKCS1_V1_5,
  AlgorithmSpec'
  #-}
