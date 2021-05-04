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

import qualified Network.AWS.Prelude as Prelude

newtype ScalingStatusCode = ScalingStatusCode'
  { fromScalingStatusCode ::
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
