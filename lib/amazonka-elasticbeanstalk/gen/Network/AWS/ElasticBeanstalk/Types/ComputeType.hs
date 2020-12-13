{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ComputeType
  ( ComputeType
      ( ComputeType',
        BuildGENERAL1Small,
        BuildGENERAL1Medium,
        BuildGENERAL1Large
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ComputeType = ComputeType' Lude.Text
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

pattern BuildGENERAL1Small :: ComputeType
pattern BuildGENERAL1Small = ComputeType' "BUILD_GENERAL1_SMALL"

pattern BuildGENERAL1Medium :: ComputeType
pattern BuildGENERAL1Medium = ComputeType' "BUILD_GENERAL1_MEDIUM"

pattern BuildGENERAL1Large :: ComputeType
pattern BuildGENERAL1Large = ComputeType' "BUILD_GENERAL1_LARGE"

{-# COMPLETE
  BuildGENERAL1Small,
  BuildGENERAL1Medium,
  BuildGENERAL1Large,
  ComputeType'
  #-}
