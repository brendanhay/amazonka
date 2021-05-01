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
-- Module      : Network.AWS.DynamoDB.Types.ReturnValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnValue
  ( ReturnValue
      ( ..,
        ReturnValue_ALL_NEW,
        ReturnValue_ALL_OLD,
        ReturnValue_NONE,
        ReturnValue_UPDATED_NEW,
        ReturnValue_UPDATED_OLD
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReturnValue = ReturnValue'
  { fromReturnValue ::
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

pattern ReturnValue_ALL_NEW :: ReturnValue
pattern ReturnValue_ALL_NEW = ReturnValue' "ALL_NEW"

pattern ReturnValue_ALL_OLD :: ReturnValue
pattern ReturnValue_ALL_OLD = ReturnValue' "ALL_OLD"

pattern ReturnValue_NONE :: ReturnValue
pattern ReturnValue_NONE = ReturnValue' "NONE"

pattern ReturnValue_UPDATED_NEW :: ReturnValue
pattern ReturnValue_UPDATED_NEW = ReturnValue' "UPDATED_NEW"

pattern ReturnValue_UPDATED_OLD :: ReturnValue
pattern ReturnValue_UPDATED_OLD = ReturnValue' "UPDATED_OLD"

{-# COMPLETE
  ReturnValue_ALL_NEW,
  ReturnValue_ALL_OLD,
  ReturnValue_NONE,
  ReturnValue_UPDATED_NEW,
  ReturnValue_UPDATED_OLD,
  ReturnValue'
  #-}
