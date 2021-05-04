{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ModelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ModelStatus
  ( ModelStatus
      ( ..,
        ModelStatus_DELETING,
        ModelStatus_IN_ERROR,
        ModelStatus_STOPPED,
        ModelStatus_STOP_REQUESTED,
        ModelStatus_SUBMITTED,
        ModelStatus_TRAINED,
        ModelStatus_TRAINING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ModelStatus = ModelStatus'
  { fromModelStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ModelStatus_DELETING :: ModelStatus
pattern ModelStatus_DELETING = ModelStatus' "DELETING"

pattern ModelStatus_IN_ERROR :: ModelStatus
pattern ModelStatus_IN_ERROR = ModelStatus' "IN_ERROR"

pattern ModelStatus_STOPPED :: ModelStatus
pattern ModelStatus_STOPPED = ModelStatus' "STOPPED"

pattern ModelStatus_STOP_REQUESTED :: ModelStatus
pattern ModelStatus_STOP_REQUESTED = ModelStatus' "STOP_REQUESTED"

pattern ModelStatus_SUBMITTED :: ModelStatus
pattern ModelStatus_SUBMITTED = ModelStatus' "SUBMITTED"

pattern ModelStatus_TRAINED :: ModelStatus
pattern ModelStatus_TRAINED = ModelStatus' "TRAINED"

pattern ModelStatus_TRAINING :: ModelStatus
pattern ModelStatus_TRAINING = ModelStatus' "TRAINING"

{-# COMPLETE
  ModelStatus_DELETING,
  ModelStatus_IN_ERROR,
  ModelStatus_STOPPED,
  ModelStatus_STOP_REQUESTED,
  ModelStatus_SUBMITTED,
  ModelStatus_TRAINED,
  ModelStatus_TRAINING,
  ModelStatus'
  #-}
