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
-- Module      : Amazonka.CodeDeploy.Types.FileExistsBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.FileExistsBehavior
  ( FileExistsBehavior
      ( ..,
        FileExistsBehavior_DISALLOW,
        FileExistsBehavior_OVERWRITE,
        FileExistsBehavior_RETAIN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FileExistsBehavior = FileExistsBehavior'
  { fromFileExistsBehavior ::
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

pattern FileExistsBehavior_DISALLOW :: FileExistsBehavior
pattern FileExistsBehavior_DISALLOW = FileExistsBehavior' "DISALLOW"

pattern FileExistsBehavior_OVERWRITE :: FileExistsBehavior
pattern FileExistsBehavior_OVERWRITE = FileExistsBehavior' "OVERWRITE"

pattern FileExistsBehavior_RETAIN :: FileExistsBehavior
pattern FileExistsBehavior_RETAIN = FileExistsBehavior' "RETAIN"

{-# COMPLETE
  FileExistsBehavior_DISALLOW,
  FileExistsBehavior_OVERWRITE,
  FileExistsBehavior_RETAIN,
  FileExistsBehavior'
  #-}
