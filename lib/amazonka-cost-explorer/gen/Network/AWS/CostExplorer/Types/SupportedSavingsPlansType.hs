-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SupportedSavingsPlansType
  ( SupportedSavingsPlansType
      ( SupportedSavingsPlansType',
        ComputeSp,
        EC2InstanceSp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SupportedSavingsPlansType = SupportedSavingsPlansType' Lude.Text
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

pattern ComputeSp :: SupportedSavingsPlansType
pattern ComputeSp = SupportedSavingsPlansType' "COMPUTE_SP"

pattern EC2InstanceSp :: SupportedSavingsPlansType
pattern EC2InstanceSp = SupportedSavingsPlansType' "EC2_INSTANCE_SP"

{-# COMPLETE
  ComputeSp,
  EC2InstanceSp,
  SupportedSavingsPlansType'
  #-}
