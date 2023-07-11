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
-- Module      : Amazonka.QLDB.Types.ErrorCause
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.ErrorCause
  ( ErrorCause
      ( ..,
        ErrorCause_IAM_PERMISSION_REVOKED,
        ErrorCause_KINESIS_STREAM_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCause = ErrorCause'
  { fromErrorCause ::
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

pattern ErrorCause_IAM_PERMISSION_REVOKED :: ErrorCause
pattern ErrorCause_IAM_PERMISSION_REVOKED = ErrorCause' "IAM_PERMISSION_REVOKED"

pattern ErrorCause_KINESIS_STREAM_NOT_FOUND :: ErrorCause
pattern ErrorCause_KINESIS_STREAM_NOT_FOUND = ErrorCause' "KINESIS_STREAM_NOT_FOUND"

{-# COMPLETE
  ErrorCause_IAM_PERMISSION_REVOKED,
  ErrorCause_KINESIS_STREAM_NOT_FOUND,
  ErrorCause'
  #-}
