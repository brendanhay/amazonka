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
-- Module      : Network.AWS.EMR.Types.ScaleDownBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScaleDownBehavior
  ( ScaleDownBehavior
      ( ..,
        ScaleDownBehavior_TERMINATE_AT_INSTANCE_HOUR,
        ScaleDownBehavior_TERMINATE_AT_TASK_COMPLETION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ScaleDownBehavior = ScaleDownBehavior'
  { fromScaleDownBehavior ::
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

pattern ScaleDownBehavior_TERMINATE_AT_INSTANCE_HOUR :: ScaleDownBehavior
pattern ScaleDownBehavior_TERMINATE_AT_INSTANCE_HOUR = ScaleDownBehavior' "TERMINATE_AT_INSTANCE_HOUR"

pattern ScaleDownBehavior_TERMINATE_AT_TASK_COMPLETION :: ScaleDownBehavior
pattern ScaleDownBehavior_TERMINATE_AT_TASK_COMPLETION = ScaleDownBehavior' "TERMINATE_AT_TASK_COMPLETION"

{-# COMPLETE
  ScaleDownBehavior_TERMINATE_AT_INSTANCE_HOUR,
  ScaleDownBehavior_TERMINATE_AT_TASK_COMPLETION,
  ScaleDownBehavior'
  #-}
