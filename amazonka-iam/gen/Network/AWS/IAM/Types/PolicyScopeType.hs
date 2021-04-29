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
-- Module      : Network.AWS.IAM.Types.PolicyScopeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyScopeType
  ( PolicyScopeType
      ( ..,
        PolicyScopeType_AWS,
        PolicyScopeType_All,
        PolicyScopeType_Local
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PolicyScopeType = PolicyScopeType'
  { fromPolicyScopeType ::
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

pattern PolicyScopeType_AWS :: PolicyScopeType
pattern PolicyScopeType_AWS = PolicyScopeType' "AWS"

pattern PolicyScopeType_All :: PolicyScopeType
pattern PolicyScopeType_All = PolicyScopeType' "All"

pattern PolicyScopeType_Local :: PolicyScopeType
pattern PolicyScopeType_Local = PolicyScopeType' "Local"

{-# COMPLETE
  PolicyScopeType_AWS,
  PolicyScopeType_All,
  PolicyScopeType_Local,
  PolicyScopeType'
  #-}
