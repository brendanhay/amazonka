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
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
  ( CloudWatchLogsInitialPosition
      ( ..,
        CloudWatchLogsInitialPosition_End_of_file,
        CloudWatchLogsInitialPosition_Start_of_file
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies where to start to read data (start_of_file or end_of_file).
-- The default is start_of_file. It\'s only used if there is no state
-- persisted for that log stream.
newtype CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition'
  { fromCloudWatchLogsInitialPosition ::
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

pattern CloudWatchLogsInitialPosition_End_of_file :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPosition_End_of_file = CloudWatchLogsInitialPosition' "end_of_file"

pattern CloudWatchLogsInitialPosition_Start_of_file :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPosition_Start_of_file = CloudWatchLogsInitialPosition' "start_of_file"

{-# COMPLETE
  CloudWatchLogsInitialPosition_End_of_file,
  CloudWatchLogsInitialPosition_Start_of_file,
  CloudWatchLogsInitialPosition'
  #-}
