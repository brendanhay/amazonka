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
-- Module      : Amazonka.Lambda.Types.ResponseStreamingInvocationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ResponseStreamingInvocationType
  ( ResponseStreamingInvocationType
      ( ..,
        ResponseStreamingInvocationType_DryRun,
        ResponseStreamingInvocationType_RequestResponse
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResponseStreamingInvocationType = ResponseStreamingInvocationType'
  { fromResponseStreamingInvocationType ::
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

pattern ResponseStreamingInvocationType_DryRun :: ResponseStreamingInvocationType
pattern ResponseStreamingInvocationType_DryRun = ResponseStreamingInvocationType' "DryRun"

pattern ResponseStreamingInvocationType_RequestResponse :: ResponseStreamingInvocationType
pattern ResponseStreamingInvocationType_RequestResponse = ResponseStreamingInvocationType' "RequestResponse"

{-# COMPLETE
  ResponseStreamingInvocationType_DryRun,
  ResponseStreamingInvocationType_RequestResponse,
  ResponseStreamingInvocationType'
  #-}
