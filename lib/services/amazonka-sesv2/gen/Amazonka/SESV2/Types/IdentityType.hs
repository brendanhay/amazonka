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
-- Module      : Amazonka.SESV2.Types.IdentityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.IdentityType
  ( IdentityType
      ( ..,
        IdentityType_DOMAIN,
        IdentityType_EMAIL_ADDRESS,
        IdentityType_MANAGED_DOMAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IdentityType = IdentityType'
  { fromIdentityType ::
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

pattern IdentityType_DOMAIN :: IdentityType
pattern IdentityType_DOMAIN = IdentityType' "DOMAIN"

pattern IdentityType_EMAIL_ADDRESS :: IdentityType
pattern IdentityType_EMAIL_ADDRESS = IdentityType' "EMAIL_ADDRESS"

pattern IdentityType_MANAGED_DOMAIN :: IdentityType
pattern IdentityType_MANAGED_DOMAIN = IdentityType' "MANAGED_DOMAIN"

{-# COMPLETE
  IdentityType_DOMAIN,
  IdentityType_EMAIL_ADDRESS,
  IdentityType_MANAGED_DOMAIN,
  IdentityType'
  #-}
