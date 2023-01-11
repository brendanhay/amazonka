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
-- Module      : Amazonka.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryType
  ( DirectoryType
      ( ..,
        DirectoryType_ADConnector,
        DirectoryType_MicrosoftAD,
        DirectoryType_SharedMicrosoftAD,
        DirectoryType_SimpleAD
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

pattern DirectoryType_ADConnector :: DirectoryType
pattern DirectoryType_ADConnector = DirectoryType' "ADConnector"

pattern DirectoryType_MicrosoftAD :: DirectoryType
pattern DirectoryType_MicrosoftAD = DirectoryType' "MicrosoftAD"

pattern DirectoryType_SharedMicrosoftAD :: DirectoryType
pattern DirectoryType_SharedMicrosoftAD = DirectoryType' "SharedMicrosoftAD"

pattern DirectoryType_SimpleAD :: DirectoryType
pattern DirectoryType_SimpleAD = DirectoryType' "SimpleAD"

{-# COMPLETE
  DirectoryType_ADConnector,
  DirectoryType_MicrosoftAD,
  DirectoryType_SharedMicrosoftAD,
  DirectoryType_SimpleAD,
  DirectoryType'
  #-}
