{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
  ( HyperParameterTuningJobObjectiveType
      ( HyperParameterTuningJobObjectiveType',
        HyperParameterTuningJobObjectiveTypeMaximize,
        HyperParameterTuningJobObjectiveTypeMinimize,
        fromHyperParameterTuningJobObjectiveType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype HyperParameterTuningJobObjectiveType = HyperParameterTuningJobObjectiveType'
  { fromHyperParameterTuningJobObjectiveType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern HyperParameterTuningJobObjectiveTypeMaximize :: HyperParameterTuningJobObjectiveType
pattern HyperParameterTuningJobObjectiveTypeMaximize = HyperParameterTuningJobObjectiveType' "Maximize"

pattern HyperParameterTuningJobObjectiveTypeMinimize :: HyperParameterTuningJobObjectiveType
pattern HyperParameterTuningJobObjectiveTypeMinimize = HyperParameterTuningJobObjectiveType' "Minimize"

{-# COMPLETE
  HyperParameterTuningJobObjectiveTypeMaximize,
  HyperParameterTuningJobObjectiveTypeMinimize,
  HyperParameterTuningJobObjectiveType'
  #-}
