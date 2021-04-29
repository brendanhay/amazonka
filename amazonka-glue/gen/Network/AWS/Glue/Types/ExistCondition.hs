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
-- Module      : Network.AWS.Glue.Types.ExistCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExistCondition
  ( ExistCondition
      ( ..,
        ExistCondition_MUST_EXIST,
        ExistCondition_NONE,
        ExistCondition_NOT_EXIST
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ExistCondition = ExistCondition'
  { fromExistCondition ::
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

pattern ExistCondition_MUST_EXIST :: ExistCondition
pattern ExistCondition_MUST_EXIST = ExistCondition' "MUST_EXIST"

pattern ExistCondition_NONE :: ExistCondition
pattern ExistCondition_NONE = ExistCondition' "NONE"

pattern ExistCondition_NOT_EXIST :: ExistCondition
pattern ExistCondition_NOT_EXIST = ExistCondition' "NOT_EXIST"

{-# COMPLETE
  ExistCondition_MUST_EXIST,
  ExistCondition_NONE,
  ExistCondition_NOT_EXIST,
  ExistCondition'
  #-}
