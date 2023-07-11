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
-- Module      : Amazonka.CodeGuruReviewer.Types.EncryptionOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.EncryptionOption
  ( EncryptionOption
      ( ..,
        EncryptionOption_AWS_OWNED_CMK,
        EncryptionOption_CUSTOMER_MANAGED_CMK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EncryptionOption = EncryptionOption'
  { fromEncryptionOption ::
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

pattern EncryptionOption_AWS_OWNED_CMK :: EncryptionOption
pattern EncryptionOption_AWS_OWNED_CMK = EncryptionOption' "AWS_OWNED_CMK"

pattern EncryptionOption_CUSTOMER_MANAGED_CMK :: EncryptionOption
pattern EncryptionOption_CUSTOMER_MANAGED_CMK = EncryptionOption' "CUSTOMER_MANAGED_CMK"

{-# COMPLETE
  EncryptionOption_AWS_OWNED_CMK,
  EncryptionOption_CUSTOMER_MANAGED_CMK,
  EncryptionOption'
  #-}
