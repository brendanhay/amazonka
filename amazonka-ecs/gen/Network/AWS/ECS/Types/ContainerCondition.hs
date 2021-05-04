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
-- Module      : Network.AWS.ECS.Types.ContainerCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerCondition
  ( ContainerCondition
      ( ..,
        ContainerCondition_COMPLETE,
        ContainerCondition_HEALTHY,
        ContainerCondition_START,
        ContainerCondition_SUCCESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ContainerCondition = ContainerCondition'
  { fromContainerCondition ::
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
