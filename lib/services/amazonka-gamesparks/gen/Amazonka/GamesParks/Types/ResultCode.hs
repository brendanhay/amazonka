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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResultCode = ResultCode'
  { fromResultCode ::
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
