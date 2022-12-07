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
-- Module      : Amazonka.MechanicalTurk.Types.Comparator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.Comparator
  ( Comparator
      ( ..,
        Comparator_DoesNotExist,
        Comparator_EqualTo,
        Comparator_Exists,
        Comparator_GreaterThan,
        Comparator_GreaterThanOrEqualTo,
        Comparator_In,
        Comparator_LessThan,
        Comparator_LessThanOrEqualTo,
        Comparator_NotEqualTo,
        Comparator_NotIn
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Comparator = Comparator'
  { fromComparator ::
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

pattern Comparator_DoesNotExist :: Comparator
pattern Comparator_DoesNotExist = Comparator' "DoesNotExist"

pattern Comparator_EqualTo :: Comparator
pattern Comparator_EqualTo = Comparator' "EqualTo"

pattern Comparator_Exists :: Comparator
pattern Comparator_Exists = Comparator' "Exists"

pattern Comparator_GreaterThan :: Comparator
pattern Comparator_GreaterThan = Comparator' "GreaterThan"

pattern Comparator_GreaterThanOrEqualTo :: Comparator
pattern Comparator_GreaterThanOrEqualTo = Comparator' "GreaterThanOrEqualTo"

pattern Comparator_In :: Comparator
pattern Comparator_In = Comparator' "In"

pattern Comparator_LessThan :: Comparator
pattern Comparator_LessThan = Comparator' "LessThan"

pattern Comparator_LessThanOrEqualTo :: Comparator
pattern Comparator_LessThanOrEqualTo = Comparator' "LessThanOrEqualTo"

pattern Comparator_NotEqualTo :: Comparator
pattern Comparator_NotEqualTo = Comparator' "NotEqualTo"

pattern Comparator_NotIn :: Comparator
pattern Comparator_NotIn = Comparator' "NotIn"

{-# COMPLETE
  Comparator_DoesNotExist,
  Comparator_EqualTo,
  Comparator_Exists,
  Comparator_GreaterThan,
  Comparator_GreaterThanOrEqualTo,
  Comparator_In,
  Comparator_LessThan,
  Comparator_LessThanOrEqualTo,
  Comparator_NotEqualTo,
  Comparator_NotIn,
  Comparator'
  #-}
