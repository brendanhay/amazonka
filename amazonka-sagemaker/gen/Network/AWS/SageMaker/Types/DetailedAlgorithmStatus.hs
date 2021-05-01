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
-- Module      : Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
  ( DetailedAlgorithmStatus
      ( ..,
        DetailedAlgorithmStatus_Completed,
        DetailedAlgorithmStatus_Failed,
        DetailedAlgorithmStatus_InProgress,
        DetailedAlgorithmStatus_NotStarted
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DetailedAlgorithmStatus = DetailedAlgorithmStatus'
  { fromDetailedAlgorithmStatus ::
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

pattern DetailedAlgorithmStatus_Completed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Completed = DetailedAlgorithmStatus' "Completed"

pattern DetailedAlgorithmStatus_Failed :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_Failed = DetailedAlgorithmStatus' "Failed"

pattern DetailedAlgorithmStatus_InProgress :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_InProgress = DetailedAlgorithmStatus' "InProgress"

pattern DetailedAlgorithmStatus_NotStarted :: DetailedAlgorithmStatus
pattern DetailedAlgorithmStatus_NotStarted = DetailedAlgorithmStatus' "NotStarted"

{-# COMPLETE
  DetailedAlgorithmStatus_Completed,
  DetailedAlgorithmStatus_Failed,
  DetailedAlgorithmStatus_InProgress,
  DetailedAlgorithmStatus_NotStarted,
  DetailedAlgorithmStatus'
  #-}
