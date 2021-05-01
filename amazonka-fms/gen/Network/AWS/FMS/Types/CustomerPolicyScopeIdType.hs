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
-- Module      : Network.AWS.FMS.Types.CustomerPolicyScopeIdType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.CustomerPolicyScopeIdType
  ( CustomerPolicyScopeIdType
      ( ..,
        CustomerPolicyScopeIdType_ACCOUNT,
        CustomerPolicyScopeIdType_ORG_UNIT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CustomerPolicyScopeIdType = CustomerPolicyScopeIdType'
  { fromCustomerPolicyScopeIdType ::
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

pattern CustomerPolicyScopeIdType_ACCOUNT :: CustomerPolicyScopeIdType
pattern CustomerPolicyScopeIdType_ACCOUNT = CustomerPolicyScopeIdType' "ACCOUNT"

pattern CustomerPolicyScopeIdType_ORG_UNIT :: CustomerPolicyScopeIdType
pattern CustomerPolicyScopeIdType_ORG_UNIT = CustomerPolicyScopeIdType' "ORG_UNIT"

{-# COMPLETE
  CustomerPolicyScopeIdType_ACCOUNT,
  CustomerPolicyScopeIdType_ORG_UNIT,
  CustomerPolicyScopeIdType'
  #-}
