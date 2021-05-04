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
-- Module      : Network.AWS.Config.Types.AggregatedSourceStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregatedSourceStatusType
  ( AggregatedSourceStatusType
      ( ..,
        AggregatedSourceStatusType_FAILED,
        AggregatedSourceStatusType_OUTDATED,
        AggregatedSourceStatusType_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AggregatedSourceStatusType = AggregatedSourceStatusType'
  { fromAggregatedSourceStatusType ::
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

pattern AggregatedSourceStatusType_FAILED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_FAILED = AggregatedSourceStatusType' "FAILED"

pattern AggregatedSourceStatusType_OUTDATED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_OUTDATED = AggregatedSourceStatusType' "OUTDATED"

pattern AggregatedSourceStatusType_SUCCEEDED :: AggregatedSourceStatusType
pattern AggregatedSourceStatusType_SUCCEEDED = AggregatedSourceStatusType' "SUCCEEDED"

{-# COMPLETE
  AggregatedSourceStatusType_FAILED,
  AggregatedSourceStatusType_OUTDATED,
  AggregatedSourceStatusType_SUCCEEDED,
  AggregatedSourceStatusType'
  #-}
