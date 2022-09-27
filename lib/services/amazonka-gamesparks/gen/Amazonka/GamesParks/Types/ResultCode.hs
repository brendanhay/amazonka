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
-- Module      : Amazonka.GamesParks.Types.ResultCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.ResultCode
  ( ResultCode
      ( ..,
        ResultCode_INVALID_ROLE_FAILURE,
        ResultCode_SUCCESS,
        ResultCode_UNSPECIFIED_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ResultCode = ResultCode'
  { fromResultCode ::
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

pattern ResultCode_INVALID_ROLE_FAILURE :: ResultCode
pattern ResultCode_INVALID_ROLE_FAILURE = ResultCode' "INVALID_ROLE_FAILURE"

pattern ResultCode_SUCCESS :: ResultCode
pattern ResultCode_SUCCESS = ResultCode' "SUCCESS"

pattern ResultCode_UNSPECIFIED_FAILURE :: ResultCode
pattern ResultCode_UNSPECIFIED_FAILURE = ResultCode' "UNSPECIFIED_FAILURE"

{-# COMPLETE
  ResultCode_INVALID_ROLE_FAILURE,
  ResultCode_SUCCESS,
  ResultCode_UNSPECIFIED_FAILURE,
  ResultCode'
  #-}
