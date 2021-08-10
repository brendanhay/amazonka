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
-- Module      : Network.AWS.Glacier.Types.FileHeaderInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.FileHeaderInfo
  ( FileHeaderInfo
      ( ..,
        FileHeaderInfo_IGNORE,
        FileHeaderInfo_NONE,
        FileHeaderInfo_USE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FileHeaderInfo = FileHeaderInfo'
  { fromFileHeaderInfo ::
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

pattern FileHeaderInfo_IGNORE :: FileHeaderInfo
pattern FileHeaderInfo_IGNORE = FileHeaderInfo' "IGNORE"

pattern FileHeaderInfo_NONE :: FileHeaderInfo
pattern FileHeaderInfo_NONE = FileHeaderInfo' "NONE"

pattern FileHeaderInfo_USE :: FileHeaderInfo
pattern FileHeaderInfo_USE = FileHeaderInfo' "USE"

{-# COMPLETE
  FileHeaderInfo_IGNORE,
  FileHeaderInfo_NONE,
  FileHeaderInfo_USE,
  FileHeaderInfo'
  #-}
