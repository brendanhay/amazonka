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
-- Module      : Amazonka.Amplify.Types.JobType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.JobType
  ( JobType
      ( ..,
        JobType_MANUAL,
        JobType_RELEASE,
        JobType_RETRY,
        JobType_WEB_HOOK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobType = JobType' {fromJobType :: Data.Text}
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

pattern JobType_MANUAL :: JobType
pattern JobType_MANUAL = JobType' "MANUAL"

pattern JobType_RELEASE :: JobType
pattern JobType_RELEASE = JobType' "RELEASE"

pattern JobType_RETRY :: JobType
pattern JobType_RETRY = JobType' "RETRY"

pattern JobType_WEB_HOOK :: JobType
pattern JobType_WEB_HOOK = JobType' "WEB_HOOK"

{-# COMPLETE
  JobType_MANUAL,
  JobType_RELEASE,
  JobType_RETRY,
  JobType_WEB_HOOK,
  JobType'
  #-}
