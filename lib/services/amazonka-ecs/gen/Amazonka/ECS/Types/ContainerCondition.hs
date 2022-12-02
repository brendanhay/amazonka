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
-- Module      : Amazonka.ECS.Types.ContainerCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerCondition
  ( ContainerCondition
      ( ..,
        ContainerCondition_COMPLETE,
        ContainerCondition_HEALTHY,
        ContainerCondition_START,
        ContainerCondition_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerCondition = ContainerCondition'
  { fromContainerCondition ::
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

pattern ContainerCondition_COMPLETE :: ContainerCondition
pattern ContainerCondition_COMPLETE = ContainerCondition' "COMPLETE"

pattern ContainerCondition_HEALTHY :: ContainerCondition
pattern ContainerCondition_HEALTHY = ContainerCondition' "HEALTHY"

pattern ContainerCondition_START :: ContainerCondition
pattern ContainerCondition_START = ContainerCondition' "START"

pattern ContainerCondition_SUCCESS :: ContainerCondition
pattern ContainerCondition_SUCCESS = ContainerCondition' "SUCCESS"

{-# COMPLETE
  ContainerCondition_COMPLETE,
  ContainerCondition_HEALTHY,
  ContainerCondition_START,
  ContainerCondition_SUCCESS,
  ContainerCondition'
  #-}
