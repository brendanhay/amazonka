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
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimitType
  ( ReusableDelegationSetLimitType
      ( ..,
        ReusableDelegationSetLimitType_MAX_ZONES_BY_REUSABLE_DELEGATION_SET
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype ReusableDelegationSetLimitType = ReusableDelegationSetLimitType'
  { fromReusableDelegationSetLimitType ::
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

pattern ReusableDelegationSetLimitType_MAX_ZONES_BY_REUSABLE_DELEGATION_SET :: ReusableDelegationSetLimitType
pattern ReusableDelegationSetLimitType_MAX_ZONES_BY_REUSABLE_DELEGATION_SET = ReusableDelegationSetLimitType' "MAX_ZONES_BY_REUSABLE_DELEGATION_SET"

{-# COMPLETE
  ReusableDelegationSetLimitType_MAX_ZONES_BY_REUSABLE_DELEGATION_SET,
  ReusableDelegationSetLimitType'
  #-}
