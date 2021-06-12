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
-- Module      : Network.AWS.SageMaker.Types.ObjectiveStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ObjectiveStatus
  ( ObjectiveStatus
      ( ..,
        ObjectiveStatus_Failed,
        ObjectiveStatus_Pending,
        ObjectiveStatus_Succeeded
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ObjectiveStatus = ObjectiveStatus'
  { fromObjectiveStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ObjectiveStatus_Failed :: ObjectiveStatus
pattern ObjectiveStatus_Failed = ObjectiveStatus' "Failed"

pattern ObjectiveStatus_Pending :: ObjectiveStatus
pattern ObjectiveStatus_Pending = ObjectiveStatus' "Pending"

pattern ObjectiveStatus_Succeeded :: ObjectiveStatus
pattern ObjectiveStatus_Succeeded = ObjectiveStatus' "Succeeded"

{-# COMPLETE
  ObjectiveStatus_Failed,
  ObjectiveStatus_Pending,
  ObjectiveStatus_Succeeded,
  ObjectiveStatus'
  #-}
