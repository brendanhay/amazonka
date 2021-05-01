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
-- Module      : Network.AWS.EC2.Types.StatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StatusType
  ( StatusType
      ( ..,
        StatusType_Failed,
        StatusType_Initializing,
        StatusType_Insufficient_data,
        StatusType_Passed
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype StatusType = StatusType'
  { fromStatusType ::
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

pattern StatusType_Failed :: StatusType
pattern StatusType_Failed = StatusType' "failed"

pattern StatusType_Initializing :: StatusType
pattern StatusType_Initializing = StatusType' "initializing"

pattern StatusType_Insufficient_data :: StatusType
pattern StatusType_Insufficient_data = StatusType' "insufficient-data"

pattern StatusType_Passed :: StatusType
pattern StatusType_Passed = StatusType' "passed"

{-# COMPLETE
  StatusType_Failed,
  StatusType_Initializing,
  StatusType_Insufficient_data,
  StatusType_Passed,
  StatusType'
  #-}
