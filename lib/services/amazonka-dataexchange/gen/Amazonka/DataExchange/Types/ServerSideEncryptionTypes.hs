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
-- Module      : Amazonka.DataExchange.Types.ServerSideEncryptionTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ServerSideEncryptionTypes
  ( ServerSideEncryptionTypes
      ( ..,
        ServerSideEncryptionTypes_AES256,
        ServerSideEncryptionTypes_Aws_kms
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ServerSideEncryptionTypes = ServerSideEncryptionTypes'
  { fromServerSideEncryptionTypes ::
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

pattern ServerSideEncryptionTypes_AES256 :: ServerSideEncryptionTypes
pattern ServerSideEncryptionTypes_AES256 = ServerSideEncryptionTypes' "AES256"

pattern ServerSideEncryptionTypes_Aws_kms :: ServerSideEncryptionTypes
pattern ServerSideEncryptionTypes_Aws_kms = ServerSideEncryptionTypes' "aws:kms"

{-# COMPLETE
  ServerSideEncryptionTypes_AES256,
  ServerSideEncryptionTypes_Aws_kms,
  ServerSideEncryptionTypes'
  #-}
