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
-- Module      : Amazonka.ResourceGroups.Types.QueryErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.QueryErrorCode
  ( QueryErrorCode
      ( ..,
        QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE,
        QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype QueryErrorCode = QueryErrorCode'
  { fromQueryErrorCode ::
      Core.Text
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

pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE = QueryErrorCode' "CLOUDFORMATION_STACK_INACTIVE"

pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING = QueryErrorCode' "CLOUDFORMATION_STACK_NOT_EXISTING"

{-# COMPLETE
  QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE,
  QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING,
  QueryErrorCode'
  #-}
