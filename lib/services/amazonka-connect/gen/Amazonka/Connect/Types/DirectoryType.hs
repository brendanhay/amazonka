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
-- Module      : Amazonka.Connect.Types.DirectoryType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.DirectoryType
  ( DirectoryType
      ( ..,
        DirectoryType_CONNECT_MANAGED,
        DirectoryType_EXISTING_DIRECTORY,
        DirectoryType_SAML
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DirectoryType = DirectoryType'
  { fromDirectoryType ::
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
