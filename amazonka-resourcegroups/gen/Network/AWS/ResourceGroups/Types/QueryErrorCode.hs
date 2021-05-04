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
-- Module      : Network.AWS.ResourceGroups.Types.QueryErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.QueryErrorCode
  ( QueryErrorCode
      ( ..,
        QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE,
        QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype QueryErrorCode = QueryErrorCode'
  { fromQueryErrorCode ::
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

pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE = QueryErrorCode' "CLOUDFORMATION_STACK_INACTIVE"

pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING = QueryErrorCode' "CLOUDFORMATION_STACK_NOT_EXISTING"

{-# COMPLETE
  QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE,
  QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING,
  QueryErrorCode'
  #-}
