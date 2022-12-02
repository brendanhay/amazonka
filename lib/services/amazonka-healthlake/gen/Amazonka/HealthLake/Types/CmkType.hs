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
-- Module      : Amazonka.HealthLake.Types.CmkType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.CmkType
  ( CmkType
      ( ..,
        CmkType_AWS_OWNED_KMS_KEY,
        CmkType_CUSTOMER_MANAGED_KMS_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CmkType = CmkType' {fromCmkType :: Data.Text}
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

pattern CmkType_AWS_OWNED_KMS_KEY :: CmkType
pattern CmkType_AWS_OWNED_KMS_KEY = CmkType' "AWS_OWNED_KMS_KEY"

pattern CmkType_CUSTOMER_MANAGED_KMS_KEY :: CmkType
pattern CmkType_CUSTOMER_MANAGED_KMS_KEY = CmkType' "CUSTOMER_MANAGED_KMS_KEY"

{-# COMPLETE
  CmkType_AWS_OWNED_KMS_KEY,
  CmkType_CUSTOMER_MANAGED_KMS_KEY,
  CmkType'
  #-}
