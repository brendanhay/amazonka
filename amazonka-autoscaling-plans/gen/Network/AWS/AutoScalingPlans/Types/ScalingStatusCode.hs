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
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
  ( ScalingStatusCode
      ( ..,
        ScalingStatusCode_Active,
        ScalingStatusCode_Inactive,
        ScalingStatusCode_PartiallyActive
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScalingStatusCode = ScalingStatusCode'
  { fromScalingStatusCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ScalingStatusCode_Active :: ScalingStatusCode
pattern ScalingStatusCode_Active = ScalingStatusCode' "Active"

pattern ScalingStatusCode_Inactive :: ScalingStatusCode
pattern ScalingStatusCode_Inactive = ScalingStatusCode' "Inactive"

pattern ScalingStatusCode_PartiallyActive :: ScalingStatusCode
pattern ScalingStatusCode_PartiallyActive = ScalingStatusCode' "PartiallyActive"

{-# COMPLETE
  ScalingStatusCode_Active,
  ScalingStatusCode_Inactive,
  ScalingStatusCode_PartiallyActive,
  ScalingStatusCode'
  #-}
