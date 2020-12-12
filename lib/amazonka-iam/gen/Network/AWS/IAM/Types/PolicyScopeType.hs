{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyScopeType
  ( PolicyScopeType
      ( PolicyScopeType',
        AWS,
        All,
        Local
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PolicyScopeType = PolicyScopeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWS :: PolicyScopeType
pattern AWS = PolicyScopeType' "AWS"

pattern All :: PolicyScopeType
pattern All = PolicyScopeType' "All"

pattern Local :: PolicyScopeType
pattern Local = PolicyScopeType' "Local"

{-# COMPLETE
  AWS,
  All,
  Local,
  PolicyScopeType'
  #-}
