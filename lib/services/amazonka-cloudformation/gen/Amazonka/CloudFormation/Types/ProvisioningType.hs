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
-- Module      : Amazonka.CloudFormation.Types.ProvisioningType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ProvisioningType
  ( ProvisioningType
      ( ..,
        ProvisioningType_FULLY_MUTABLE,
        ProvisioningType_IMMUTABLE,
        ProvisioningType_NON_PROVISIONABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProvisioningType = ProvisioningType'
  { fromProvisioningType ::
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

pattern ProvisioningType_FULLY_MUTABLE :: ProvisioningType
pattern ProvisioningType_FULLY_MUTABLE = ProvisioningType' "FULLY_MUTABLE"

pattern ProvisioningType_IMMUTABLE :: ProvisioningType
pattern ProvisioningType_IMMUTABLE = ProvisioningType' "IMMUTABLE"

pattern ProvisioningType_NON_PROVISIONABLE :: ProvisioningType
pattern ProvisioningType_NON_PROVISIONABLE = ProvisioningType' "NON_PROVISIONABLE"

{-# COMPLETE
  ProvisioningType_FULLY_MUTABLE,
  ProvisioningType_IMMUTABLE,
  ProvisioningType_NON_PROVISIONABLE,
  ProvisioningType'
  #-}
