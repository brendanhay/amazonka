{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
  ( NotebookInstanceAcceleratorType
    ( NotebookInstanceAcceleratorType'
    , NotebookInstanceAcceleratorTypeMl_EIA1_Medium
    , NotebookInstanceAcceleratorTypeMl_EIA1_Large
    , NotebookInstanceAcceleratorTypeMl_EIA1_Xlarge
    , NotebookInstanceAcceleratorTypeMl_EIA2_Medium
    , NotebookInstanceAcceleratorTypeMl_EIA2_Large
    , NotebookInstanceAcceleratorTypeMl_EIA2_Xlarge
    , fromNotebookInstanceAcceleratorType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NotebookInstanceAcceleratorType = NotebookInstanceAcceleratorType'{fromNotebookInstanceAcceleratorType
                                                                           :: Core.Text}
                                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                            Core.Generic)
                                            deriving newtype (Core.IsString, Core.Hashable,
                                                              Core.NFData, Core.ToJSONKey,
                                                              Core.FromJSONKey, Core.ToJSON,
                                                              Core.FromJSON, Core.ToXML,
                                                              Core.FromXML, Core.ToText,
                                                              Core.FromText, Core.ToByteString,
                                                              Core.ToQuery, Core.ToHeader)

pattern NotebookInstanceAcceleratorTypeMl_EIA1_Medium :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA1_Medium = NotebookInstanceAcceleratorType' "ml.eia1.medium"

pattern NotebookInstanceAcceleratorTypeMl_EIA1_Large :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA1_Large = NotebookInstanceAcceleratorType' "ml.eia1.large"

pattern NotebookInstanceAcceleratorTypeMl_EIA1_Xlarge :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA1_Xlarge = NotebookInstanceAcceleratorType' "ml.eia1.xlarge"

pattern NotebookInstanceAcceleratorTypeMl_EIA2_Medium :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA2_Medium = NotebookInstanceAcceleratorType' "ml.eia2.medium"

pattern NotebookInstanceAcceleratorTypeMl_EIA2_Large :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA2_Large = NotebookInstanceAcceleratorType' "ml.eia2.large"

pattern NotebookInstanceAcceleratorTypeMl_EIA2_Xlarge :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorTypeMl_EIA2_Xlarge = NotebookInstanceAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE 
  NotebookInstanceAcceleratorTypeMl_EIA1_Medium,

  NotebookInstanceAcceleratorTypeMl_EIA1_Large,

  NotebookInstanceAcceleratorTypeMl_EIA1_Xlarge,

  NotebookInstanceAcceleratorTypeMl_EIA2_Medium,

  NotebookInstanceAcceleratorTypeMl_EIA2_Large,

  NotebookInstanceAcceleratorTypeMl_EIA2_Xlarge,
  NotebookInstanceAcceleratorType'
  #-}
