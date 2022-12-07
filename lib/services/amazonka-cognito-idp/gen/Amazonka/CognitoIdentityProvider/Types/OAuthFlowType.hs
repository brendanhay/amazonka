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
-- Module      : Amazonka.CognitoIdentityProvider.Types.OAuthFlowType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.OAuthFlowType
  ( OAuthFlowType
      ( ..,
        OAuthFlowType_Client_credentials,
        OAuthFlowType_Code,
        OAuthFlowType_Implicit
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OAuthFlowType = OAuthFlowType'
  { fromOAuthFlowType ::
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
