{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.VariantPropertyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantPropertyType
  ( VariantPropertyType
      ( VariantPropertyType',
        DataCaptureConfig,
        DesiredInstanceCount,
        DesiredWeight
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VariantPropertyType = VariantPropertyType' Lude.Text
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

pattern DataCaptureConfig :: VariantPropertyType
pattern DataCaptureConfig = VariantPropertyType' "DataCaptureConfig"

pattern DesiredInstanceCount :: VariantPropertyType
pattern DesiredInstanceCount = VariantPropertyType' "DesiredInstanceCount"

pattern DesiredWeight :: VariantPropertyType
pattern DesiredWeight = VariantPropertyType' "DesiredWeight"

{-# COMPLETE
  DataCaptureConfig,
  DesiredInstanceCount,
  DesiredWeight,
  VariantPropertyType'
  #-}
