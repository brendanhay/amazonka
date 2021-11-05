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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.ApplicationRestoreType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.ApplicationRestoreType
  ( ApplicationRestoreType
      ( ..,
        ApplicationRestoreType_RESTORE_FROM_CUSTOM_SNAPSHOT,
        ApplicationRestoreType_RESTORE_FROM_LATEST_SNAPSHOT,
        ApplicationRestoreType_SKIP_RESTORE_FROM_SNAPSHOT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ApplicationRestoreType = ApplicationRestoreType'
  { fromApplicationRestoreType ::
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

pattern ApplicationRestoreType_RESTORE_FROM_CUSTOM_SNAPSHOT :: ApplicationRestoreType
pattern ApplicationRestoreType_RESTORE_FROM_CUSTOM_SNAPSHOT = ApplicationRestoreType' "RESTORE_FROM_CUSTOM_SNAPSHOT"

pattern ApplicationRestoreType_RESTORE_FROM_LATEST_SNAPSHOT :: ApplicationRestoreType
pattern ApplicationRestoreType_RESTORE_FROM_LATEST_SNAPSHOT = ApplicationRestoreType' "RESTORE_FROM_LATEST_SNAPSHOT"

pattern ApplicationRestoreType_SKIP_RESTORE_FROM_SNAPSHOT :: ApplicationRestoreType
pattern ApplicationRestoreType_SKIP_RESTORE_FROM_SNAPSHOT = ApplicationRestoreType' "SKIP_RESTORE_FROM_SNAPSHOT"

{-# COMPLETE
  ApplicationRestoreType_RESTORE_FROM_CUSTOM_SNAPSHOT,
  ApplicationRestoreType_RESTORE_FROM_LATEST_SNAPSHOT,
  ApplicationRestoreType_SKIP_RESTORE_FROM_SNAPSHOT,
  ApplicationRestoreType'
  #-}
