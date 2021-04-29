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
-- Module      : Network.AWS.EC2.Types.AnalysisStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AnalysisStatus
  ( AnalysisStatus
      ( ..,
        AnalysisStatus_Failed,
        AnalysisStatus_Running,
        AnalysisStatus_Succeeded
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype AnalysisStatus = AnalysisStatus'
  { fromAnalysisStatus ::
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

pattern AnalysisStatus_Failed :: AnalysisStatus
pattern AnalysisStatus_Failed = AnalysisStatus' "failed"

pattern AnalysisStatus_Running :: AnalysisStatus
pattern AnalysisStatus_Running = AnalysisStatus' "running"

pattern AnalysisStatus_Succeeded :: AnalysisStatus
pattern AnalysisStatus_Succeeded = AnalysisStatus' "succeeded"

{-# COMPLETE
  AnalysisStatus_Failed,
  AnalysisStatus_Running,
  AnalysisStatus_Succeeded,
  AnalysisStatus'
  #-}
