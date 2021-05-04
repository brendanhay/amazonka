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
-- Module      : Network.AWS.SSM.Types.AssociationFilterOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilterOperatorType
  ( AssociationFilterOperatorType
      ( ..,
        AssociationFilterOperatorType_EQUAL,
        AssociationFilterOperatorType_GREATER_THAN,
        AssociationFilterOperatorType_LESS_THAN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AssociationFilterOperatorType = AssociationFilterOperatorType'
  { fromAssociationFilterOperatorType ::
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

pattern AssociationFilterOperatorType_EQUAL :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_EQUAL = AssociationFilterOperatorType' "EQUAL"

pattern AssociationFilterOperatorType_GREATER_THAN :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_GREATER_THAN = AssociationFilterOperatorType' "GREATER_THAN"

pattern AssociationFilterOperatorType_LESS_THAN :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_LESS_THAN = AssociationFilterOperatorType' "LESS_THAN"

{-# COMPLETE
  AssociationFilterOperatorType_EQUAL,
  AssociationFilterOperatorType_GREATER_THAN,
  AssociationFilterOperatorType_LESS_THAN,
  AssociationFilterOperatorType'
  #-}
