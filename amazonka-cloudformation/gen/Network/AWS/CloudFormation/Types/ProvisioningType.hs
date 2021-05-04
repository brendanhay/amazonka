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

import qualified Network.AWS.Prelude as Prelude

newtype ProvisioningType = ProvisioningType'
  { fromProvisioningType ::
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
