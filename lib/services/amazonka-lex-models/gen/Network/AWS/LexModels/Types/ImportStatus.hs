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
-- Module      : Network.AWS.LexModels.Types.ImportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ImportStatus
  ( ImportStatus
      ( ..,
        ImportStatus_COMPLETE,
        ImportStatus_FAILED,
        ImportStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ImportStatus = ImportStatus'
  { fromImportStatus ::
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

pattern ImportStatus_COMPLETE :: ImportStatus
pattern ImportStatus_COMPLETE = ImportStatus' "COMPLETE"

pattern ImportStatus_FAILED :: ImportStatus
pattern ImportStatus_FAILED = ImportStatus' "FAILED"

pattern ImportStatus_IN_PROGRESS :: ImportStatus
pattern ImportStatus_IN_PROGRESS = ImportStatus' "IN_PROGRESS"

{-# COMPLETE
  ImportStatus_COMPLETE,
  ImportStatus_FAILED,
  ImportStatus_IN_PROGRESS,
  ImportStatus'
  #-}
