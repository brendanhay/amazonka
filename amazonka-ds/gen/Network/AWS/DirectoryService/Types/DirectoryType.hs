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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryType
  ( DirectoryType
      ( ..,
        DirectoryType_ADConnector,
        DirectoryType_MicrosoftAD,
        DirectoryType_SharedMicrosoftAD,
        DirectoryType_SimpleAD
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
