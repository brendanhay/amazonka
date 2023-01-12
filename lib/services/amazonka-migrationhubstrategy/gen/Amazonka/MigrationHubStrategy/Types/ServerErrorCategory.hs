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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerErrorCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerErrorCategory
  ( ServerErrorCategory
      ( ..,
        ServerErrorCategory_ARCHITECTURE_ERROR,
        ServerErrorCategory_CONNECTIVITY_ERROR,
        ServerErrorCategory_CREDENTIAL_ERROR,
        ServerErrorCategory_OTHER_ERROR,
        ServerErrorCategory_PERMISSION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerErrorCategory = ServerErrorCategory'
  { fromServerErrorCategory ::
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

pattern ServerErrorCategory_ARCHITECTURE_ERROR :: ServerErrorCategory
pattern ServerErrorCategory_ARCHITECTURE_ERROR = ServerErrorCategory' "ARCHITECTURE_ERROR"

pattern ServerErrorCategory_CONNECTIVITY_ERROR :: ServerErrorCategory
pattern ServerErrorCategory_CONNECTIVITY_ERROR = ServerErrorCategory' "CONNECTIVITY_ERROR"

pattern ServerErrorCategory_CREDENTIAL_ERROR :: ServerErrorCategory
pattern ServerErrorCategory_CREDENTIAL_ERROR = ServerErrorCategory' "CREDENTIAL_ERROR"

pattern ServerErrorCategory_OTHER_ERROR :: ServerErrorCategory
pattern ServerErrorCategory_OTHER_ERROR = ServerErrorCategory' "OTHER_ERROR"

pattern ServerErrorCategory_PERMISSION_ERROR :: ServerErrorCategory
pattern ServerErrorCategory_PERMISSION_ERROR = ServerErrorCategory' "PERMISSION_ERROR"

{-# COMPLETE
  ServerErrorCategory_ARCHITECTURE_ERROR,
  ServerErrorCategory_CONNECTIVITY_ERROR,
  ServerErrorCategory_CREDENTIAL_ERROR,
  ServerErrorCategory_OTHER_ERROR,
  ServerErrorCategory_PERMISSION_ERROR,
  ServerErrorCategory'
  #-}
