{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
  ( NotebookInstanceAcceleratorType
      ( ..,
        NotebookInstanceAcceleratorType_Ml_eia1_large,
        NotebookInstanceAcceleratorType_Ml_eia1_medium,
        NotebookInstanceAcceleratorType_Ml_eia1_xlarge,
        NotebookInstanceAcceleratorType_Ml_eia2_large,
        NotebookInstanceAcceleratorType_Ml_eia2_medium,
        NotebookInstanceAcceleratorType_Ml_eia2_xlarge
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype NotebookInstanceAcceleratorType = NotebookInstanceAcceleratorType'
  { fromNotebookInstanceAcceleratorType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern NotebookInstanceAcceleratorType_Ml_eia1_large :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia1_large = NotebookInstanceAcceleratorType' "ml.eia1.large"

pattern NotebookInstanceAcceleratorType_Ml_eia1_medium :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia1_medium = NotebookInstanceAcceleratorType' "ml.eia1.medium"

pattern NotebookInstanceAcceleratorType_Ml_eia1_xlarge :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia1_xlarge = NotebookInstanceAcceleratorType' "ml.eia1.xlarge"

pattern NotebookInstanceAcceleratorType_Ml_eia2_large :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia2_large = NotebookInstanceAcceleratorType' "ml.eia2.large"

pattern NotebookInstanceAcceleratorType_Ml_eia2_medium :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia2_medium = NotebookInstanceAcceleratorType' "ml.eia2.medium"

pattern NotebookInstanceAcceleratorType_Ml_eia2_xlarge :: NotebookInstanceAcceleratorType
pattern NotebookInstanceAcceleratorType_Ml_eia2_xlarge = NotebookInstanceAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  NotebookInstanceAcceleratorType_Ml_eia1_large,
  NotebookInstanceAcceleratorType_Ml_eia1_medium,
  NotebookInstanceAcceleratorType_Ml_eia1_xlarge,
  NotebookInstanceAcceleratorType_Ml_eia2_large,
  NotebookInstanceAcceleratorType_Ml_eia2_medium,
  NotebookInstanceAcceleratorType_Ml_eia2_xlarge,
  NotebookInstanceAcceleratorType'
  #-}
