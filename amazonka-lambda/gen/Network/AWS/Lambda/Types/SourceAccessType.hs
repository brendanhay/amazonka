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
-- Module      : Network.AWS.Lambda.Types.SourceAccessType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SourceAccessType
  ( SourceAccessType
      ( ..,
        SourceAccessType_BASIC_AUTH,
        SourceAccessType_SASL_SCRAM_256_AUTH,
        SourceAccessType_SASL_SCRAM_512_AUTH,
        SourceAccessType_VPC_SECURITY_GROUP,
        SourceAccessType_VPC_SUBNET
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SourceAccessType = SourceAccessType'
  { fromSourceAccessType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SourceAccessType_BASIC_AUTH :: SourceAccessType
pattern SourceAccessType_BASIC_AUTH = SourceAccessType' "BASIC_AUTH"

pattern SourceAccessType_SASL_SCRAM_256_AUTH :: SourceAccessType
pattern SourceAccessType_SASL_SCRAM_256_AUTH = SourceAccessType' "SASL_SCRAM_256_AUTH"

pattern SourceAccessType_SASL_SCRAM_512_AUTH :: SourceAccessType
pattern SourceAccessType_SASL_SCRAM_512_AUTH = SourceAccessType' "SASL_SCRAM_512_AUTH"

pattern SourceAccessType_VPC_SECURITY_GROUP :: SourceAccessType
pattern SourceAccessType_VPC_SECURITY_GROUP = SourceAccessType' "VPC_SECURITY_GROUP"

pattern SourceAccessType_VPC_SUBNET :: SourceAccessType
pattern SourceAccessType_VPC_SUBNET = SourceAccessType' "VPC_SUBNET"

{-# COMPLETE
  SourceAccessType_BASIC_AUTH,
  SourceAccessType_SASL_SCRAM_256_AUTH,
  SourceAccessType_SASL_SCRAM_512_AUTH,
  SourceAccessType_VPC_SECURITY_GROUP,
  SourceAccessType_VPC_SUBNET,
  SourceAccessType'
  #-}
