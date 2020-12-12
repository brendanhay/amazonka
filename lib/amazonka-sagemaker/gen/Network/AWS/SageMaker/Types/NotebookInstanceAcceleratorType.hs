{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
  ( NotebookInstanceAcceleratorType
      ( NotebookInstanceAcceleratorType',
        Ml_EIA1_Large,
        Ml_EIA1_Medium,
        Ml_EIA1_XLarge,
        Ml_EIA2_Large,
        Ml_EIA2_Medium,
        Ml_EIA2_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotebookInstanceAcceleratorType = NotebookInstanceAcceleratorType' Lude.Text
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

pattern Ml_EIA1_Large :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_Large = NotebookInstanceAcceleratorType' "ml.eia1.large"

pattern Ml_EIA1_Medium :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_Medium = NotebookInstanceAcceleratorType' "ml.eia1.medium"

pattern Ml_EIA1_XLarge :: NotebookInstanceAcceleratorType
pattern Ml_EIA1_XLarge = NotebookInstanceAcceleratorType' "ml.eia1.xlarge"

pattern Ml_EIA2_Large :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_Large = NotebookInstanceAcceleratorType' "ml.eia2.large"

pattern Ml_EIA2_Medium :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_Medium = NotebookInstanceAcceleratorType' "ml.eia2.medium"

pattern Ml_EIA2_XLarge :: NotebookInstanceAcceleratorType
pattern Ml_EIA2_XLarge = NotebookInstanceAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  Ml_EIA1_Large,
  Ml_EIA1_Medium,
  Ml_EIA1_XLarge,
  Ml_EIA2_Large,
  Ml_EIA2_Medium,
  Ml_EIA2_XLarge,
  NotebookInstanceAcceleratorType'
  #-}
