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
-- Module      : Network.AWS.AppStream.Types.AuthenticationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AuthenticationType
  ( AuthenticationType
      ( ..,
        AuthenticationType_API,
        AuthenticationType_SAML,
        AuthenticationType_USERPOOL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthenticationType = AuthenticationType'
  { fromAuthenticationType ::
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

pattern AuthenticationType_API :: AuthenticationType
pattern AuthenticationType_API = AuthenticationType' "API"

pattern AuthenticationType_SAML :: AuthenticationType
pattern AuthenticationType_SAML = AuthenticationType' "SAML"

pattern AuthenticationType_USERPOOL :: AuthenticationType
pattern AuthenticationType_USERPOOL = AuthenticationType' "USERPOOL"

{-# COMPLETE
  AuthenticationType_API,
  AuthenticationType_SAML,
  AuthenticationType_USERPOOL,
  AuthenticationType'
  #-}
