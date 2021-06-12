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
-- Module      : Network.AWS.Glue.Types.Comparator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Comparator
  ( Comparator
      ( ..,
        Comparator_EQUALS,
        Comparator_GREATER_THAN,
        Comparator_GREATER_THAN_EQUALS,
        Comparator_LESS_THAN,
        Comparator_LESS_THAN_EQUALS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Comparator = Comparator'
  { fromComparator ::
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

pattern Comparator_EQUALS :: Comparator
pattern Comparator_EQUALS = Comparator' "EQUALS"

pattern Comparator_GREATER_THAN :: Comparator
pattern Comparator_GREATER_THAN = Comparator' "GREATER_THAN"

pattern Comparator_GREATER_THAN_EQUALS :: Comparator
pattern Comparator_GREATER_THAN_EQUALS = Comparator' "GREATER_THAN_EQUALS"

pattern Comparator_LESS_THAN :: Comparator
pattern Comparator_LESS_THAN = Comparator' "LESS_THAN"

pattern Comparator_LESS_THAN_EQUALS :: Comparator
pattern Comparator_LESS_THAN_EQUALS = Comparator' "LESS_THAN_EQUALS"

{-# COMPLETE
  Comparator_EQUALS,
  Comparator_GREATER_THAN,
  Comparator_GREATER_THAN_EQUALS,
  Comparator_LESS_THAN,
  Comparator_LESS_THAN_EQUALS,
  Comparator'
  #-}
