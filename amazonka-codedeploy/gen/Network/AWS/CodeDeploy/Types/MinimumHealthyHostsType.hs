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
-- Module      : Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
  ( MinimumHealthyHostsType
      ( ..,
        MinimumHealthyHostsType_FLEET_PERCENT,
        MinimumHealthyHostsType_HOST_COUNT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MinimumHealthyHostsType = MinimumHealthyHostsType'
  { fromMinimumHealthyHostsType ::
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

pattern MinimumHealthyHostsType_FLEET_PERCENT :: MinimumHealthyHostsType
pattern MinimumHealthyHostsType_FLEET_PERCENT = MinimumHealthyHostsType' "FLEET_PERCENT"

pattern MinimumHealthyHostsType_HOST_COUNT :: MinimumHealthyHostsType
pattern MinimumHealthyHostsType_HOST_COUNT = MinimumHealthyHostsType' "HOST_COUNT"

{-# COMPLETE
  MinimumHealthyHostsType_FLEET_PERCENT,
  MinimumHealthyHostsType_HOST_COUNT,
  MinimumHealthyHostsType'
  #-}
