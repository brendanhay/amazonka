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
-- Module      : Amazonka.DrS.Types.JobType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.JobType
  ( JobType
      ( ..,
        JobType_CREATE_CONVERTED_SNAPSHOT,
        JobType_LAUNCH,
        JobType_TERMINATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype JobType = JobType' {fromJobType :: Core.Text}
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

pattern JobType_CREATE_CONVERTED_SNAPSHOT :: JobType
pattern JobType_CREATE_CONVERTED_SNAPSHOT = JobType' "CREATE_CONVERTED_SNAPSHOT"

pattern JobType_LAUNCH :: JobType
pattern JobType_LAUNCH = JobType' "LAUNCH"

pattern JobType_TERMINATE :: JobType
pattern JobType_TERMINATE = JobType' "TERMINATE"

{-# COMPLETE
  JobType_CREATE_CONVERTED_SNAPSHOT,
  JobType_LAUNCH,
  JobType_TERMINATE,
  JobType'
  #-}
