{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.AuthType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.AuthType
  ( AuthType
      ( AuthType',
        ATOauth,
        ATBasicAuth,
        ATPersonalAccessToken
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthType = AuthType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ATOauth :: AuthType
pattern ATOauth = AuthType' "OAUTH"

pattern ATBasicAuth :: AuthType
pattern ATBasicAuth = AuthType' "BASIC_AUTH"

pattern ATPersonalAccessToken :: AuthType
pattern ATPersonalAccessToken = AuthType' "PERSONAL_ACCESS_TOKEN"

{-# COMPLETE
  ATOauth,
  ATBasicAuth,
  ATPersonalAccessToken,
  AuthType'
  #-}
