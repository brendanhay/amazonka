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
-- Module      : Amazonka.CodeCommit.Types.ObjectTypeEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ObjectTypeEnum
  ( ObjectTypeEnum
      ( ..,
        ObjectTypeEnum_DIRECTORY,
        ObjectTypeEnum_FILE,
        ObjectTypeEnum_GIT_LINK,
        ObjectTypeEnum_SYMBOLIC_LINK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ObjectTypeEnum = ObjectTypeEnum'
  { fromObjectTypeEnum ::
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

pattern ObjectTypeEnum_DIRECTORY :: ObjectTypeEnum
pattern ObjectTypeEnum_DIRECTORY = ObjectTypeEnum' "DIRECTORY"

pattern ObjectTypeEnum_FILE :: ObjectTypeEnum
pattern ObjectTypeEnum_FILE = ObjectTypeEnum' "FILE"

pattern ObjectTypeEnum_GIT_LINK :: ObjectTypeEnum
pattern ObjectTypeEnum_GIT_LINK = ObjectTypeEnum' "GIT_LINK"

pattern ObjectTypeEnum_SYMBOLIC_LINK :: ObjectTypeEnum
pattern ObjectTypeEnum_SYMBOLIC_LINK = ObjectTypeEnum' "SYMBOLIC_LINK"

{-# COMPLETE
  ObjectTypeEnum_DIRECTORY,
  ObjectTypeEnum_FILE,
  ObjectTypeEnum_GIT_LINK,
  ObjectTypeEnum_SYMBOLIC_LINK,
  ObjectTypeEnum'
  #-}
