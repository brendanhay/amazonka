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
-- Module      : Amazonka.AccessAnalyzer.Types.AccessPreviewStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AccessPreviewStatus
  ( AccessPreviewStatus
      ( ..,
        AccessPreviewStatus_COMPLETED,
        AccessPreviewStatus_CREATING,
        AccessPreviewStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AccessPreviewStatus = AccessPreviewStatus'
  { fromAccessPreviewStatus ::
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

pattern AccessPreviewStatus_COMPLETED :: AccessPreviewStatus
pattern AccessPreviewStatus_COMPLETED = AccessPreviewStatus' "COMPLETED"

pattern AccessPreviewStatus_CREATING :: AccessPreviewStatus
pattern AccessPreviewStatus_CREATING = AccessPreviewStatus' "CREATING"

pattern AccessPreviewStatus_FAILED :: AccessPreviewStatus
pattern AccessPreviewStatus_FAILED = AccessPreviewStatus' "FAILED"

{-# COMPLETE
  AccessPreviewStatus_COMPLETED,
  AccessPreviewStatus_CREATING,
  AccessPreviewStatus_FAILED,
  AccessPreviewStatus'
  #-}
