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
-- Module      : Amazonka.PaymentCryptography.Types.WrappedKeyMaterialFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.WrappedKeyMaterialFormat
  ( WrappedKeyMaterialFormat
      ( ..,
        WrappedKeyMaterialFormat_KEY_CRYPTOGRAM,
        WrappedKeyMaterialFormat_TR31_KEY_BLOCK,
        WrappedKeyMaterialFormat_TR34_KEY_BLOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WrappedKeyMaterialFormat = WrappedKeyMaterialFormat'
  { fromWrappedKeyMaterialFormat ::
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

pattern WrappedKeyMaterialFormat_KEY_CRYPTOGRAM :: WrappedKeyMaterialFormat
pattern WrappedKeyMaterialFormat_KEY_CRYPTOGRAM = WrappedKeyMaterialFormat' "KEY_CRYPTOGRAM"

pattern WrappedKeyMaterialFormat_TR31_KEY_BLOCK :: WrappedKeyMaterialFormat
pattern WrappedKeyMaterialFormat_TR31_KEY_BLOCK = WrappedKeyMaterialFormat' "TR31_KEY_BLOCK"

pattern WrappedKeyMaterialFormat_TR34_KEY_BLOCK :: WrappedKeyMaterialFormat
pattern WrappedKeyMaterialFormat_TR34_KEY_BLOCK = WrappedKeyMaterialFormat' "TR34_KEY_BLOCK"

{-# COMPLETE
  WrappedKeyMaterialFormat_KEY_CRYPTOGRAM,
  WrappedKeyMaterialFormat_TR31_KEY_BLOCK,
  WrappedKeyMaterialFormat_TR34_KEY_BLOCK,
  WrappedKeyMaterialFormat'
  #-}
