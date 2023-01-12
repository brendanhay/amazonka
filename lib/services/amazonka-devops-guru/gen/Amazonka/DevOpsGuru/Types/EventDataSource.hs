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
-- Module      : Amazonka.DevOpsGuru.Types.EventDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EventDataSource
  ( EventDataSource
      ( ..,
        EventDataSource_AWS_CLOUD_TRAIL,
        EventDataSource_AWS_CODE_DEPLOY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventDataSource = EventDataSource'
  { fromEventDataSource ::
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

pattern EventDataSource_AWS_CLOUD_TRAIL :: EventDataSource
pattern EventDataSource_AWS_CLOUD_TRAIL = EventDataSource' "AWS_CLOUD_TRAIL"

pattern EventDataSource_AWS_CODE_DEPLOY :: EventDataSource
pattern EventDataSource_AWS_CODE_DEPLOY = EventDataSource' "AWS_CODE_DEPLOY"

{-# COMPLETE
  EventDataSource_AWS_CLOUD_TRAIL,
  EventDataSource_AWS_CODE_DEPLOY,
  EventDataSource'
  #-}
