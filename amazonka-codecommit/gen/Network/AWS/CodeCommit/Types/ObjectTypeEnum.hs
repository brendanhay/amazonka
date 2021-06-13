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
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypeEnum
  ( ObjectTypeEnum
      ( ..,
        ObjectTypeEnum_DIRECTORY,
        ObjectTypeEnum_FILE,
        ObjectTypeEnum_GIT_LINK,
        ObjectTypeEnum_SYMBOLIC_LINK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ObjectTypeEnum = ObjectTypeEnum'
  { fromObjectTypeEnum ::
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
