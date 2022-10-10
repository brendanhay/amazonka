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
-- Module      : Amazonka.Glue.Types.SourceControlProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SourceControlProvider
  ( SourceControlProvider
      ( ..,
        SourceControlProvider_AWS_CODE_COMMIT,
        SourceControlProvider_GITHUB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SourceControlProvider = SourceControlProvider'
  { fromSourceControlProvider ::
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

pattern SourceControlProvider_AWS_CODE_COMMIT :: SourceControlProvider
pattern SourceControlProvider_AWS_CODE_COMMIT = SourceControlProvider' "AWS_CODE_COMMIT"

pattern SourceControlProvider_GITHUB :: SourceControlProvider
pattern SourceControlProvider_GITHUB = SourceControlProvider' "GITHUB"

{-# COMPLETE
  SourceControlProvider_AWS_CODE_COMMIT,
  SourceControlProvider_GITHUB,
  SourceControlProvider'
  #-}
