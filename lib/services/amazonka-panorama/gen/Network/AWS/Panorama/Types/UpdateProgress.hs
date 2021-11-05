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
-- Module      : Amazonka.Panorama.Types.UpdateProgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.UpdateProgress
  ( UpdateProgress
      ( ..,
        UpdateProgress_COMPLETED,
        UpdateProgress_DOWNLOADING,
        UpdateProgress_FAILED,
        UpdateProgress_IN_PROGRESS,
        UpdateProgress_PENDING,
        UpdateProgress_REBOOTING,
        UpdateProgress_VERIFYING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UpdateProgress = UpdateProgress'
  { fromUpdateProgress ::
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

pattern UpdateProgress_COMPLETED :: UpdateProgress
pattern UpdateProgress_COMPLETED = UpdateProgress' "COMPLETED"

pattern UpdateProgress_DOWNLOADING :: UpdateProgress
pattern UpdateProgress_DOWNLOADING = UpdateProgress' "DOWNLOADING"

pattern UpdateProgress_FAILED :: UpdateProgress
pattern UpdateProgress_FAILED = UpdateProgress' "FAILED"

pattern UpdateProgress_IN_PROGRESS :: UpdateProgress
pattern UpdateProgress_IN_PROGRESS = UpdateProgress' "IN_PROGRESS"

pattern UpdateProgress_PENDING :: UpdateProgress
pattern UpdateProgress_PENDING = UpdateProgress' "PENDING"

pattern UpdateProgress_REBOOTING :: UpdateProgress
pattern UpdateProgress_REBOOTING = UpdateProgress' "REBOOTING"

pattern UpdateProgress_VERIFYING :: UpdateProgress
pattern UpdateProgress_VERIFYING = UpdateProgress' "VERIFYING"

{-# COMPLETE
  UpdateProgress_COMPLETED,
  UpdateProgress_DOWNLOADING,
  UpdateProgress_FAILED,
  UpdateProgress_IN_PROGRESS,
  UpdateProgress_PENDING,
  UpdateProgress_REBOOTING,
  UpdateProgress_VERIFYING,
  UpdateProgress'
  #-}
