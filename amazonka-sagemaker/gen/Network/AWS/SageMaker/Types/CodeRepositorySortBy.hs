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
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySortBy
  ( CodeRepositorySortBy
      ( ..,
        CodeRepositorySortBy_CreationTime,
        CodeRepositorySortBy_LastModifiedTime,
        CodeRepositorySortBy_Name
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CodeRepositorySortBy = CodeRepositorySortBy'
  { fromCodeRepositorySortBy ::
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
