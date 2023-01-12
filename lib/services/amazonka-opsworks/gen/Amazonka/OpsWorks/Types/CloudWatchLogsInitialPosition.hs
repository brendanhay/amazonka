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
-- Module      : Amazonka.OpsWorks.Types.CloudWatchLogsInitialPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsInitialPosition
  ( CloudWatchLogsInitialPosition
      ( ..,
        CloudWatchLogsInitialPosition_End_of_file,
        CloudWatchLogsInitialPosition_Start_of_file
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies where to start to read data (start_of_file or end_of_file).
-- The default is start_of_file. It\'s only used if there is no state
-- persisted for that log stream.
newtype CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition'
  { fromCloudWatchLogsInitialPosition ::
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

pattern CloudWatchLogsInitialPosition_End_of_file :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPosition_End_of_file = CloudWatchLogsInitialPosition' "end_of_file"

pattern CloudWatchLogsInitialPosition_Start_of_file :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPosition_Start_of_file = CloudWatchLogsInitialPosition' "start_of_file"

{-# COMPLETE
  CloudWatchLogsInitialPosition_End_of_file,
  CloudWatchLogsInitialPosition_Start_of_file,
  CloudWatchLogsInitialPosition'
  #-}
