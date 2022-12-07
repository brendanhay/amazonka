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
-- Module      : Amazonka.ElasticSearch.Types.PrincipalType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.PrincipalType
  ( PrincipalType
      ( ..,
        PrincipalType_AWS_ACCOUNT,
        PrincipalType_AWS_SERVICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type of AWS account permitted to manage VPC endpoints.:
--
-- -   AWS_ACCOUNT: Indicates that the account is owned by an AWS user.
-- -   AWS_SERVICE: Indicates the the account is owned by an AWS service.
newtype PrincipalType = PrincipalType'
  { fromPrincipalType ::
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

pattern PrincipalType_AWS_ACCOUNT :: PrincipalType
pattern PrincipalType_AWS_ACCOUNT = PrincipalType' "AWS_ACCOUNT"

pattern PrincipalType_AWS_SERVICE :: PrincipalType
pattern PrincipalType_AWS_SERVICE = PrincipalType' "AWS_SERVICE"

{-# COMPLETE
  PrincipalType_AWS_ACCOUNT,
  PrincipalType_AWS_SERVICE,
  PrincipalType'
  #-}
