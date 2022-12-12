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
-- Module      : Amazonka.OAM.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OAM.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS__CloudWatch__Metric,
        ResourceType_AWS__Logs__LogGroup,
        ResourceType_AWS__XRay__Trace
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_AWS__CloudWatch__Metric :: ResourceType
pattern ResourceType_AWS__CloudWatch__Metric = ResourceType' "AWS::CloudWatch::Metric"

pattern ResourceType_AWS__Logs__LogGroup :: ResourceType
pattern ResourceType_AWS__Logs__LogGroup = ResourceType' "AWS::Logs::LogGroup"

pattern ResourceType_AWS__XRay__Trace :: ResourceType
pattern ResourceType_AWS__XRay__Trace = ResourceType' "AWS::XRay::Trace"

{-# COMPLETE
  ResourceType_AWS__CloudWatch__Metric,
  ResourceType_AWS__Logs__LogGroup,
  ResourceType_AWS__XRay__Trace,
  ResourceType'
  #-}
