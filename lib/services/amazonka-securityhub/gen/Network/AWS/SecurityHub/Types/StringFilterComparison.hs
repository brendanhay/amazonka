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
-- Module      : Network.AWS.SecurityHub.Types.StringFilterComparison
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.StringFilterComparison
  ( StringFilterComparison
      ( ..,
        StringFilterComparison_EQUALS,
        StringFilterComparison_NOT_EQUALS,
        StringFilterComparison_PREFIX,
        StringFilterComparison_PREFIX_NOT_EQUALS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StringFilterComparison = StringFilterComparison'
  { fromStringFilterComparison ::
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

pattern StringFilterComparison_EQUALS :: StringFilterComparison
pattern StringFilterComparison_EQUALS = StringFilterComparison' "EQUALS"

pattern StringFilterComparison_NOT_EQUALS :: StringFilterComparison
pattern StringFilterComparison_NOT_EQUALS = StringFilterComparison' "NOT_EQUALS"

pattern StringFilterComparison_PREFIX :: StringFilterComparison
pattern StringFilterComparison_PREFIX = StringFilterComparison' "PREFIX"

pattern StringFilterComparison_PREFIX_NOT_EQUALS :: StringFilterComparison
pattern StringFilterComparison_PREFIX_NOT_EQUALS = StringFilterComparison' "PREFIX_NOT_EQUALS"

{-# COMPLETE
  StringFilterComparison_EQUALS,
  StringFilterComparison_NOT_EQUALS,
  StringFilterComparison_PREFIX,
  StringFilterComparison_PREFIX_NOT_EQUALS,
  StringFilterComparison'
  #-}
