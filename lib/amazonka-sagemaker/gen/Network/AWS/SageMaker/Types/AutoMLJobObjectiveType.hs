-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
  ( AutoMLJobObjectiveType
      ( AutoMLJobObjectiveType',
        AMLJOTMaximize,
        AMLJOTMinimize
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoMLJobObjectiveType = AutoMLJobObjectiveType' Lude.Text
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

pattern AMLJOTMaximize :: AutoMLJobObjectiveType
pattern AMLJOTMaximize = AutoMLJobObjectiveType' "Maximize"

pattern AMLJOTMinimize :: AutoMLJobObjectiveType
pattern AMLJOTMinimize = AutoMLJobObjectiveType' "Minimize"

{-# COMPLETE
  AMLJOTMaximize,
  AMLJOTMinimize,
  AutoMLJobObjectiveType'
  #-}
