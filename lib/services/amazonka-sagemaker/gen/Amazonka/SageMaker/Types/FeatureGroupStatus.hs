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
-- Module      : Amazonka.SageMaker.Types.FeatureGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureGroupStatus
  ( FeatureGroupStatus
      ( ..,
        FeatureGroupStatus_CreateFailed,
        FeatureGroupStatus_Created,
        FeatureGroupStatus_Creating,
        FeatureGroupStatus_DeleteFailed,
        FeatureGroupStatus_Deleting
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FeatureGroupStatus = FeatureGroupStatus'
  { fromFeatureGroupStatus ::
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

pattern FeatureGroupStatus_CreateFailed :: FeatureGroupStatus
pattern FeatureGroupStatus_CreateFailed = FeatureGroupStatus' "CreateFailed"

pattern FeatureGroupStatus_Created :: FeatureGroupStatus
pattern FeatureGroupStatus_Created = FeatureGroupStatus' "Created"

pattern FeatureGroupStatus_Creating :: FeatureGroupStatus
pattern FeatureGroupStatus_Creating = FeatureGroupStatus' "Creating"

pattern FeatureGroupStatus_DeleteFailed :: FeatureGroupStatus
pattern FeatureGroupStatus_DeleteFailed = FeatureGroupStatus' "DeleteFailed"

pattern FeatureGroupStatus_Deleting :: FeatureGroupStatus
pattern FeatureGroupStatus_Deleting = FeatureGroupStatus' "Deleting"

{-# COMPLETE
  FeatureGroupStatus_CreateFailed,
  FeatureGroupStatus_Created,
  FeatureGroupStatus_Creating,
  FeatureGroupStatus_DeleteFailed,
  FeatureGroupStatus_Deleting,
  FeatureGroupStatus'
  #-}
