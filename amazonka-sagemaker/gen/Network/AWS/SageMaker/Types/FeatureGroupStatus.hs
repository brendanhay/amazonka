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
-- Module      : Network.AWS.SageMaker.Types.FeatureGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FeatureGroupStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype FeatureGroupStatus = FeatureGroupStatus'
  { fromFeatureGroupStatus ::
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
