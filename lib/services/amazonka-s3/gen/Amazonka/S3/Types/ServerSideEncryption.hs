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
-- Module      : Amazonka.S3.Types.ServerSideEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ServerSideEncryption
  ( ServerSideEncryption
      ( ..,
        ServerSideEncryption_AES256,
        ServerSideEncryption_Aws_kms,
        ServerSideEncryption_Aws_kms_dsse
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ServerSideEncryption = ServerSideEncryption'
  { fromServerSideEncryption ::
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

pattern ServerSideEncryption_AES256 :: ServerSideEncryption
pattern ServerSideEncryption_AES256 = ServerSideEncryption' "AES256"

pattern ServerSideEncryption_Aws_kms :: ServerSideEncryption
pattern ServerSideEncryption_Aws_kms = ServerSideEncryption' "aws:kms"

pattern ServerSideEncryption_Aws_kms_dsse :: ServerSideEncryption
pattern ServerSideEncryption_Aws_kms_dsse = ServerSideEncryption' "aws:kms:dsse"

{-# COMPLETE
  ServerSideEncryption_AES256,
  ServerSideEncryption_Aws_kms,
  ServerSideEncryption_Aws_kms_dsse,
  ServerSideEncryption'
  #-}
