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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
  ( OAuthFlowType
      ( ..,
        OAuthFlowType_Client_credentials,
        OAuthFlowType_Code,
        OAuthFlowType_Implicit
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OAuthFlowType = OAuthFlowType'
  { fromOAuthFlowType ::
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

pattern OAuthFlowType_Client_credentials :: OAuthFlowType
pattern OAuthFlowType_Client_credentials = OAuthFlowType' "client_credentials"

pattern OAuthFlowType_Code :: OAuthFlowType
pattern OAuthFlowType_Code = OAuthFlowType' "code"

pattern OAuthFlowType_Implicit :: OAuthFlowType
pattern OAuthFlowType_Implicit = OAuthFlowType' "implicit"

{-# COMPLETE
  OAuthFlowType_Client_credentials,
  OAuthFlowType_Code,
  OAuthFlowType_Implicit,
  OAuthFlowType'
  #-}
