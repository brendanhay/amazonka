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
-- Module      : Network.AWS.Organizations.Types.CreateAccountState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountState
  ( CreateAccountState
      ( ..,
        CreateAccountState_FAILED,
        CreateAccountState_IN_PROGRESS,
        CreateAccountState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CreateAccountState = CreateAccountState'
  { fromCreateAccountState ::
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

pattern CreateAccountState_FAILED :: CreateAccountState
pattern CreateAccountState_FAILED = CreateAccountState' "FAILED"

pattern CreateAccountState_IN_PROGRESS :: CreateAccountState
pattern CreateAccountState_IN_PROGRESS = CreateAccountState' "IN_PROGRESS"

pattern CreateAccountState_SUCCEEDED :: CreateAccountState
pattern CreateAccountState_SUCCEEDED = CreateAccountState' "SUCCEEDED"

{-# COMPLETE
  CreateAccountState_FAILED,
  CreateAccountState_IN_PROGRESS,
  CreateAccountState_SUCCEEDED,
  CreateAccountState'
  #-}
