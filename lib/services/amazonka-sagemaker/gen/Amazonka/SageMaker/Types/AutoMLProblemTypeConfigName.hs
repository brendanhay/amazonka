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
-- Module      : Amazonka.SageMaker.Types.AutoMLProblemTypeConfigName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLProblemTypeConfigName
  ( AutoMLProblemTypeConfigName
      ( ..,
        AutoMLProblemTypeConfigName_ImageClassification,
        AutoMLProblemTypeConfigName_Tabular,
        AutoMLProblemTypeConfigName_TextClassification
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLProblemTypeConfigName = AutoMLProblemTypeConfigName'
  { fromAutoMLProblemTypeConfigName ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AutoMLProblemTypeConfigName_ImageClassification :: AutoMLProblemTypeConfigName
pattern AutoMLProblemTypeConfigName_ImageClassification = AutoMLProblemTypeConfigName' "ImageClassification"

pattern AutoMLProblemTypeConfigName_Tabular :: AutoMLProblemTypeConfigName
pattern AutoMLProblemTypeConfigName_Tabular = AutoMLProblemTypeConfigName' "Tabular"

pattern AutoMLProblemTypeConfigName_TextClassification :: AutoMLProblemTypeConfigName
pattern AutoMLProblemTypeConfigName_TextClassification = AutoMLProblemTypeConfigName' "TextClassification"

{-# COMPLETE
  AutoMLProblemTypeConfigName_ImageClassification,
  AutoMLProblemTypeConfigName_Tabular,
  AutoMLProblemTypeConfigName_TextClassification,
  AutoMLProblemTypeConfigName'
  #-}
