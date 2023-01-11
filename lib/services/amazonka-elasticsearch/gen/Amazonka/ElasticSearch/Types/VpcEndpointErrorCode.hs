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
-- Module      : Amazonka.ElasticSearch.Types.VpcEndpointErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.VpcEndpointErrorCode
  ( VpcEndpointErrorCode
      ( ..,
        VpcEndpointErrorCode_ENDPOINT_NOT_FOUND,
        VpcEndpointErrorCode_SERVER_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the error code of the failure encountered while describing the
-- VPC endpoint:
--
-- -   ENDPOINT_NOT_FOUND: Indicates that the requested VPC endpoint does
--     not exist.
-- -   SERVER_ERROR: Indicates the describe endpoint operation failed due
--     to an internal server error.
newtype VpcEndpointErrorCode = VpcEndpointErrorCode'
  { fromVpcEndpointErrorCode ::
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

pattern VpcEndpointErrorCode_ENDPOINT_NOT_FOUND :: VpcEndpointErrorCode
pattern VpcEndpointErrorCode_ENDPOINT_NOT_FOUND = VpcEndpointErrorCode' "ENDPOINT_NOT_FOUND"

pattern VpcEndpointErrorCode_SERVER_ERROR :: VpcEndpointErrorCode
pattern VpcEndpointErrorCode_SERVER_ERROR = VpcEndpointErrorCode' "SERVER_ERROR"

{-# COMPLETE
  VpcEndpointErrorCode_ENDPOINT_NOT_FOUND,
  VpcEndpointErrorCode_SERVER_ERROR,
  VpcEndpointErrorCode'
  #-}
