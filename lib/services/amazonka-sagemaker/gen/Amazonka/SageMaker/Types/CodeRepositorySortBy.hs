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
-- Module      : Amazonka.SageMaker.Types.CodeRepositorySortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CodeRepositorySortBy
  ( CodeRepositorySortBy
      ( ..,
        CodeRepositorySortBy_CreationTime,
        CodeRepositorySortBy_LastModifiedTime,
        CodeRepositorySortBy_Name
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CodeRepositorySortBy = CodeRepositorySortBy'
  { fromCodeRepositorySortBy ::
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

pattern CodeRepositorySortBy_CreationTime :: CodeRepositorySortBy
pattern CodeRepositorySortBy_CreationTime = CodeRepositorySortBy' "CreationTime"

pattern CodeRepositorySortBy_LastModifiedTime :: CodeRepositorySortBy
pattern CodeRepositorySortBy_LastModifiedTime = CodeRepositorySortBy' "LastModifiedTime"

pattern CodeRepositorySortBy_Name :: CodeRepositorySortBy
pattern CodeRepositorySortBy_Name = CodeRepositorySortBy' "Name"

{-# COMPLETE
  CodeRepositorySortBy_CreationTime,
  CodeRepositorySortBy_LastModifiedTime,
  CodeRepositorySortBy_Name,
  CodeRepositorySortBy'
  #-}
