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
-- Module      : Amazonka.CodeCommit.Types.RelativeFileVersionEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.RelativeFileVersionEnum
  ( RelativeFileVersionEnum
      ( ..,
        RelativeFileVersionEnum_AFTER,
        RelativeFileVersionEnum_BEFORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelativeFileVersionEnum = RelativeFileVersionEnum'
  { fromRelativeFileVersionEnum ::
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

pattern RelativeFileVersionEnum_AFTER :: RelativeFileVersionEnum
pattern RelativeFileVersionEnum_AFTER = RelativeFileVersionEnum' "AFTER"

pattern RelativeFileVersionEnum_BEFORE :: RelativeFileVersionEnum
pattern RelativeFileVersionEnum_BEFORE = RelativeFileVersionEnum' "BEFORE"

{-# COMPLETE
  RelativeFileVersionEnum_AFTER,
  RelativeFileVersionEnum_BEFORE,
  RelativeFileVersionEnum'
  #-}
