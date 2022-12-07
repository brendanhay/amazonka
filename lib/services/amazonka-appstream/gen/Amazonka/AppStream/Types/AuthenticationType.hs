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
-- Module      : Amazonka.AppStream.Types.AuthenticationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.AuthenticationType
  ( AuthenticationType
      ( ..,
        AuthenticationType_API,
        AuthenticationType_AWS_AD,
        AuthenticationType_SAML,
        AuthenticationType_USERPOOL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticationType = AuthenticationType'
  { fromAuthenticationType ::
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

pattern AuthenticationType_API :: AuthenticationType
pattern AuthenticationType_API = AuthenticationType' "API"

pattern AuthenticationType_AWS_AD :: AuthenticationType
pattern AuthenticationType_AWS_AD = AuthenticationType' "AWS_AD"

pattern AuthenticationType_SAML :: AuthenticationType
pattern AuthenticationType_SAML = AuthenticationType' "SAML"

pattern AuthenticationType_USERPOOL :: AuthenticationType
pattern AuthenticationType_USERPOOL = AuthenticationType' "USERPOOL"

{-# COMPLETE
  AuthenticationType_API,
  AuthenticationType_AWS_AD,
  AuthenticationType_SAML,
  AuthenticationType_USERPOOL,
  AuthenticationType'
  #-}
