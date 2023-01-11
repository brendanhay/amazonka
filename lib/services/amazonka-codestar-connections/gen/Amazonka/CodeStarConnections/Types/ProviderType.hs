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
-- Module      : Amazonka.CodeStarConnections.Types.ProviderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Types.ProviderType
  ( ProviderType
      ( ..,
        ProviderType_Bitbucket,
        ProviderType_GitHub,
        ProviderType_GitHubEnterpriseServer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProviderType = ProviderType'
  { fromProviderType ::
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

pattern ProviderType_Bitbucket :: ProviderType
pattern ProviderType_Bitbucket = ProviderType' "Bitbucket"

pattern ProviderType_GitHub :: ProviderType
pattern ProviderType_GitHub = ProviderType' "GitHub"

pattern ProviderType_GitHubEnterpriseServer :: ProviderType
pattern ProviderType_GitHubEnterpriseServer = ProviderType' "GitHubEnterpriseServer"

{-# COMPLETE
  ProviderType_Bitbucket,
  ProviderType_GitHub,
  ProviderType_GitHubEnterpriseServer,
  ProviderType'
  #-}
