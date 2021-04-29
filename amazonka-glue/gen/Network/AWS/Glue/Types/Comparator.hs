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

import qualified Network.AWS.Prelude as Prelude

newtype Comparator = Comparator'
  { fromComparator ::
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
