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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryErrorCode = QueryErrorCode'
  { fromQueryErrorCode ::
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

pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE = QueryErrorCode' "CLOUDFORMATION_STACK_INACTIVE"

pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING :: QueryErrorCode
pattern QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING = QueryErrorCode' "CLOUDFORMATION_STACK_NOT_EXISTING"

{-# COMPLETE
  QueryErrorCode_CLOUDFORMATION_STACK_INACTIVE,
  QueryErrorCode_CLOUDFORMATION_STACK_NOT_EXISTING,
  QueryErrorCode'
  #-}
