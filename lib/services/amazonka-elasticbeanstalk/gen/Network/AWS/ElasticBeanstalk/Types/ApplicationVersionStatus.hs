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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
  ( ApplicationVersionStatus
      ( ..,
        ApplicationVersionStatus_Building,
        ApplicationVersionStatus_Failed,
        ApplicationVersionStatus_Processed,
        ApplicationVersionStatus_Processing,
        ApplicationVersionStatus_Unprocessed
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ApplicationVersionStatus = ApplicationVersionStatus'
  { fromApplicationVersionStatus ::
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

pattern ApplicationVersionStatus_Building :: ApplicationVersionStatus
pattern ApplicationVersionStatus_Building = ApplicationVersionStatus' "Building"

pattern ApplicationVersionStatus_Failed :: ApplicationVersionStatus
pattern ApplicationVersionStatus_Failed = ApplicationVersionStatus' "Failed"

pattern ApplicationVersionStatus_Processed :: ApplicationVersionStatus
pattern ApplicationVersionStatus_Processed = ApplicationVersionStatus' "Processed"

pattern ApplicationVersionStatus_Processing :: ApplicationVersionStatus
pattern ApplicationVersionStatus_Processing = ApplicationVersionStatus' "Processing"

pattern ApplicationVersionStatus_Unprocessed :: ApplicationVersionStatus
pattern ApplicationVersionStatus_Unprocessed = ApplicationVersionStatus' "Unprocessed"

{-# COMPLETE
  ApplicationVersionStatus_Building,
  ApplicationVersionStatus_Failed,
  ApplicationVersionStatus_Processed,
  ApplicationVersionStatus_Processing,
  ApplicationVersionStatus_Unprocessed,
  ApplicationVersionStatus'
  #-}
