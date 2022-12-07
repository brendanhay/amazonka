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
-- Module      : Amazonka.CodeGuruReviewer.Types.RecommendationCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RecommendationCategory
  ( RecommendationCategory
      ( ..,
        RecommendationCategory_AWSBestPractices,
        RecommendationCategory_AWSCloudFormationIssues,
        RecommendationCategory_CodeInconsistencies,
        RecommendationCategory_CodeMaintenanceIssues,
        RecommendationCategory_ConcurrencyIssues,
        RecommendationCategory_DuplicateCode,
        RecommendationCategory_InputValidations,
        RecommendationCategory_JavaBestPractices,
        RecommendationCategory_PythonBestPractices,
        RecommendationCategory_ResourceLeaks,
        RecommendationCategory_SecurityIssues
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecommendationCategory = RecommendationCategory'
  { fromRecommendationCategory ::
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

pattern RecommendationCategory_AWSBestPractices :: RecommendationCategory
pattern RecommendationCategory_AWSBestPractices = RecommendationCategory' "AWSBestPractices"

pattern RecommendationCategory_AWSCloudFormationIssues :: RecommendationCategory
pattern RecommendationCategory_AWSCloudFormationIssues = RecommendationCategory' "AWSCloudFormationIssues"

pattern RecommendationCategory_CodeInconsistencies :: RecommendationCategory
pattern RecommendationCategory_CodeInconsistencies = RecommendationCategory' "CodeInconsistencies"

pattern RecommendationCategory_CodeMaintenanceIssues :: RecommendationCategory
pattern RecommendationCategory_CodeMaintenanceIssues = RecommendationCategory' "CodeMaintenanceIssues"

pattern RecommendationCategory_ConcurrencyIssues :: RecommendationCategory
pattern RecommendationCategory_ConcurrencyIssues = RecommendationCategory' "ConcurrencyIssues"

pattern RecommendationCategory_DuplicateCode :: RecommendationCategory
pattern RecommendationCategory_DuplicateCode = RecommendationCategory' "DuplicateCode"

pattern RecommendationCategory_InputValidations :: RecommendationCategory
pattern RecommendationCategory_InputValidations = RecommendationCategory' "InputValidations"

pattern RecommendationCategory_JavaBestPractices :: RecommendationCategory
pattern RecommendationCategory_JavaBestPractices = RecommendationCategory' "JavaBestPractices"

pattern RecommendationCategory_PythonBestPractices :: RecommendationCategory
pattern RecommendationCategory_PythonBestPractices = RecommendationCategory' "PythonBestPractices"

pattern RecommendationCategory_ResourceLeaks :: RecommendationCategory
pattern RecommendationCategory_ResourceLeaks = RecommendationCategory' "ResourceLeaks"

pattern RecommendationCategory_SecurityIssues :: RecommendationCategory
pattern RecommendationCategory_SecurityIssues = RecommendationCategory' "SecurityIssues"

{-# COMPLETE
  RecommendationCategory_AWSBestPractices,
  RecommendationCategory_AWSCloudFormationIssues,
  RecommendationCategory_CodeInconsistencies,
  RecommendationCategory_CodeMaintenanceIssues,
  RecommendationCategory_ConcurrencyIssues,
  RecommendationCategory_DuplicateCode,
  RecommendationCategory_InputValidations,
  RecommendationCategory_JavaBestPractices,
  RecommendationCategory_PythonBestPractices,
  RecommendationCategory_ResourceLeaks,
  RecommendationCategory_SecurityIssues,
  RecommendationCategory'
  #-}
