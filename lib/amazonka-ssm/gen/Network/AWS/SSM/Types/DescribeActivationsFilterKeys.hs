-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DescribeActivationsFilterKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DescribeActivationsFilterKeys
  ( DescribeActivationsFilterKeys
      ( DescribeActivationsFilterKeys',
        DAFKActivationIds,
        DAFKDefaultInstanceName,
        DAFKIAMRole
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DescribeActivationsFilterKeys = DescribeActivationsFilterKeys' Lude.Text
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

pattern DAFKActivationIds :: DescribeActivationsFilterKeys
pattern DAFKActivationIds = DescribeActivationsFilterKeys' "ActivationIds"

pattern DAFKDefaultInstanceName :: DescribeActivationsFilterKeys
pattern DAFKDefaultInstanceName = DescribeActivationsFilterKeys' "DefaultInstanceName"

pattern DAFKIAMRole :: DescribeActivationsFilterKeys
pattern DAFKIAMRole = DescribeActivationsFilterKeys' "IamRole"

{-# COMPLETE
  DAFKActivationIds,
  DAFKDefaultInstanceName,
  DAFKIAMRole,
  DescribeActivationsFilterKeys'
  #-}
