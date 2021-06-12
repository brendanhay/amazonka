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
-- Module      : Network.AWS.CloudFormation.Types.ProvisioningType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ProvisioningType
  ( ProvisioningType
      ( ..,
        ProvisioningType_FULLY_MUTABLE,
        ProvisioningType_IMMUTABLE,
        ProvisioningType_NON_PROVISIONABLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProvisioningType = ProvisioningType'
  { fromProvisioningType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
