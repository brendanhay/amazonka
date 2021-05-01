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
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatus
  ( AlgorithmStatus
      ( ..,
        AlgorithmStatus_Completed,
        AlgorithmStatus_Deleting,
        AlgorithmStatus_Failed,
        AlgorithmStatus_InProgress,
        AlgorithmStatus_Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AlgorithmStatus = AlgorithmStatus'
  { fromAlgorithmStatus ::
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

pattern AlgorithmStatus_Completed :: AlgorithmStatus
pattern AlgorithmStatus_Completed = AlgorithmStatus' "Completed"

pattern AlgorithmStatus_Deleting :: AlgorithmStatus
pattern AlgorithmStatus_Deleting = AlgorithmStatus' "Deleting"

pattern AlgorithmStatus_Failed :: AlgorithmStatus
pattern AlgorithmStatus_Failed = AlgorithmStatus' "Failed"

pattern AlgorithmStatus_InProgress :: AlgorithmStatus
pattern AlgorithmStatus_InProgress = AlgorithmStatus' "InProgress"

pattern AlgorithmStatus_Pending :: AlgorithmStatus
pattern AlgorithmStatus_Pending = AlgorithmStatus' "Pending"

{-# COMPLETE
  AlgorithmStatus_Completed,
  AlgorithmStatus_Deleting,
  AlgorithmStatus_Failed,
  AlgorithmStatus_InProgress,
  AlgorithmStatus_Pending,
  AlgorithmStatus'
  #-}
