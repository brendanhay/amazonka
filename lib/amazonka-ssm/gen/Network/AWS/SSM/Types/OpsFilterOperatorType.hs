{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilterOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilterOperatorType
  ( OpsFilterOperatorType
      ( OpsFilterOperatorType',
        Equal,
        NotEqual,
        BeginWith,
        LessThan,
        GreaterThan,
        Exists
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OpsFilterOperatorType = OpsFilterOperatorType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Equal :: OpsFilterOperatorType
pattern Equal = OpsFilterOperatorType' "Equal"

pattern NotEqual :: OpsFilterOperatorType
pattern NotEqual = OpsFilterOperatorType' "NotEqual"

pattern BeginWith :: OpsFilterOperatorType
pattern BeginWith = OpsFilterOperatorType' "BeginWith"

pattern LessThan :: OpsFilterOperatorType
pattern LessThan = OpsFilterOperatorType' "LessThan"

pattern GreaterThan :: OpsFilterOperatorType
pattern GreaterThan = OpsFilterOperatorType' "GreaterThan"

pattern Exists :: OpsFilterOperatorType
pattern Exists = OpsFilterOperatorType' "Exists"

{-# COMPLETE
  Equal,
  NotEqual,
  BeginWith,
  LessThan,
  GreaterThan,
  Exists,
  OpsFilterOperatorType'
  #-}
