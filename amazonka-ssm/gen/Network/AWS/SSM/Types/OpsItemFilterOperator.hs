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
-- Module      : Network.AWS.SSM.Types.OpsItemFilterOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilterOperator
  ( OpsItemFilterOperator
      ( ..,
        OpsItemFilterOperator_Contains,
        OpsItemFilterOperator_Equal,
        OpsItemFilterOperator_GreaterThan,
        OpsItemFilterOperator_LessThan
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OpsItemFilterOperator = OpsItemFilterOperator'
  { fromOpsItemFilterOperator ::
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

pattern OpsItemFilterOperator_Contains :: OpsItemFilterOperator
pattern OpsItemFilterOperator_Contains = OpsItemFilterOperator' "Contains"

pattern OpsItemFilterOperator_Equal :: OpsItemFilterOperator
pattern OpsItemFilterOperator_Equal = OpsItemFilterOperator' "Equal"

pattern OpsItemFilterOperator_GreaterThan :: OpsItemFilterOperator
pattern OpsItemFilterOperator_GreaterThan = OpsItemFilterOperator' "GreaterThan"

pattern OpsItemFilterOperator_LessThan :: OpsItemFilterOperator
pattern OpsItemFilterOperator_LessThan = OpsItemFilterOperator' "LessThan"

{-# COMPLETE
  OpsItemFilterOperator_Contains,
  OpsItemFilterOperator_Equal,
  OpsItemFilterOperator_GreaterThan,
  OpsItemFilterOperator_LessThan,
  OpsItemFilterOperator'
  #-}
