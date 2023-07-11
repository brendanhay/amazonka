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
-- Module      : Amazonka.EC2.Types.VerifiedAccessEndpointStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessEndpointStatusCode
  ( VerifiedAccessEndpointStatusCode
      ( ..,
        VerifiedAccessEndpointStatusCode_Active,
        VerifiedAccessEndpointStatusCode_Deleted,
        VerifiedAccessEndpointStatusCode_Deleting,
        VerifiedAccessEndpointStatusCode_Pending,
        VerifiedAccessEndpointStatusCode_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VerifiedAccessEndpointStatusCode = VerifiedAccessEndpointStatusCode'
  { fromVerifiedAccessEndpointStatusCode ::
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

pattern VerifiedAccessEndpointStatusCode_Active :: VerifiedAccessEndpointStatusCode
pattern VerifiedAccessEndpointStatusCode_Active = VerifiedAccessEndpointStatusCode' "active"

pattern VerifiedAccessEndpointStatusCode_Deleted :: VerifiedAccessEndpointStatusCode
pattern VerifiedAccessEndpointStatusCode_Deleted = VerifiedAccessEndpointStatusCode' "deleted"

pattern VerifiedAccessEndpointStatusCode_Deleting :: VerifiedAccessEndpointStatusCode
pattern VerifiedAccessEndpointStatusCode_Deleting = VerifiedAccessEndpointStatusCode' "deleting"

pattern VerifiedAccessEndpointStatusCode_Pending :: VerifiedAccessEndpointStatusCode
pattern VerifiedAccessEndpointStatusCode_Pending = VerifiedAccessEndpointStatusCode' "pending"

pattern VerifiedAccessEndpointStatusCode_Updating :: VerifiedAccessEndpointStatusCode
pattern VerifiedAccessEndpointStatusCode_Updating = VerifiedAccessEndpointStatusCode' "updating"

{-# COMPLETE
  VerifiedAccessEndpointStatusCode_Active,
  VerifiedAccessEndpointStatusCode_Deleted,
  VerifiedAccessEndpointStatusCode_Deleting,
  VerifiedAccessEndpointStatusCode_Pending,
  VerifiedAccessEndpointStatusCode_Updating,
  VerifiedAccessEndpointStatusCode'
  #-}
