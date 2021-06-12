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
-- Module      : Network.AWS.SSM.Types.InstancePatchStateOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchStateOperatorType
  ( InstancePatchStateOperatorType
      ( ..,
        InstancePatchStateOperatorType_Equal,
        InstancePatchStateOperatorType_GreaterThan,
        InstancePatchStateOperatorType_LessThan,
        InstancePatchStateOperatorType_NotEqual
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstancePatchStateOperatorType = InstancePatchStateOperatorType'
  { fromInstancePatchStateOperatorType ::
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

pattern InstancePatchStateOperatorType_Equal :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_Equal = InstancePatchStateOperatorType' "Equal"

pattern InstancePatchStateOperatorType_GreaterThan :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_GreaterThan = InstancePatchStateOperatorType' "GreaterThan"

pattern InstancePatchStateOperatorType_LessThan :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_LessThan = InstancePatchStateOperatorType' "LessThan"

pattern InstancePatchStateOperatorType_NotEqual :: InstancePatchStateOperatorType
pattern InstancePatchStateOperatorType_NotEqual = InstancePatchStateOperatorType' "NotEqual"

{-# COMPLETE
  InstancePatchStateOperatorType_Equal,
  InstancePatchStateOperatorType_GreaterThan,
  InstancePatchStateOperatorType_LessThan,
  InstancePatchStateOperatorType_NotEqual,
  InstancePatchStateOperatorType'
  #-}
