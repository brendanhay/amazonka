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
-- Module      : Network.AWS.QLDB.Types.ErrorCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.ErrorCause
  ( ErrorCause
      ( ..,
        ErrorCause_IAM_PERMISSION_REVOKED,
        ErrorCause_KINESIS_STREAM_NOT_FOUND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ErrorCause = ErrorCause'
  { fromErrorCause ::
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

pattern ErrorCause_IAM_PERMISSION_REVOKED :: ErrorCause
pattern ErrorCause_IAM_PERMISSION_REVOKED = ErrorCause' "IAM_PERMISSION_REVOKED"

pattern ErrorCause_KINESIS_STREAM_NOT_FOUND :: ErrorCause
pattern ErrorCause_KINESIS_STREAM_NOT_FOUND = ErrorCause' "KINESIS_STREAM_NOT_FOUND"

{-# COMPLETE
  ErrorCause_IAM_PERMISSION_REVOKED,
  ErrorCause_KINESIS_STREAM_NOT_FOUND,
  ErrorCause'
  #-}
