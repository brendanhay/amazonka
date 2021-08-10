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
-- Module      : Network.AWS.Connect.Types.DirectoryType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.DirectoryType
  ( DirectoryType
      ( ..,
        DirectoryType_CONNECT_MANAGED,
        DirectoryType_EXISTING_DIRECTORY,
        DirectoryType_SAML
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DirectoryType = DirectoryType'
  { fromDirectoryType ::
      Core.Text
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

pattern DirectoryType_CONNECT_MANAGED :: DirectoryType
pattern DirectoryType_CONNECT_MANAGED = DirectoryType' "CONNECT_MANAGED"

pattern DirectoryType_EXISTING_DIRECTORY :: DirectoryType
pattern DirectoryType_EXISTING_DIRECTORY = DirectoryType' "EXISTING_DIRECTORY"

pattern DirectoryType_SAML :: DirectoryType
pattern DirectoryType_SAML = DirectoryType' "SAML"

{-# COMPLETE
  DirectoryType_CONNECT_MANAGED,
  DirectoryType_EXISTING_DIRECTORY,
  DirectoryType_SAML,
  DirectoryType'
  #-}
