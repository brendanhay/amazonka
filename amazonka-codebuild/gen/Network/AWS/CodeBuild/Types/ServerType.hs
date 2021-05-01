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
-- Module      : Network.AWS.CodeBuild.Types.ServerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ServerType
  ( ServerType
      ( ..,
        ServerType_BITBUCKET,
        ServerType_GITHUB,
        ServerType_GITHUB_ENTERPRISE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ServerType = ServerType'
  { fromServerType ::
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

pattern ServerType_BITBUCKET :: ServerType
pattern ServerType_BITBUCKET = ServerType' "BITBUCKET"

pattern ServerType_GITHUB :: ServerType
pattern ServerType_GITHUB = ServerType' "GITHUB"

pattern ServerType_GITHUB_ENTERPRISE :: ServerType
pattern ServerType_GITHUB_ENTERPRISE = ServerType' "GITHUB_ENTERPRISE"

{-# COMPLETE
  ServerType_BITBUCKET,
  ServerType_GITHUB,
  ServerType_GITHUB_ENTERPRISE,
  ServerType'
  #-}
