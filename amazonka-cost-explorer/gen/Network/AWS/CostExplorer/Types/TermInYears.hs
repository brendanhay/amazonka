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
-- Module      : Network.AWS.CostExplorer.Types.TermInYears
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TermInYears
  ( TermInYears
      ( ..,
        TermInYears_ONE_YEAR,
        TermInYears_THREE_YEARS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TermInYears = TermInYears'
  { fromTermInYears ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern TermInYears_ONE_YEAR :: TermInYears
pattern TermInYears_ONE_YEAR = TermInYears' "ONE_YEAR"

pattern TermInYears_THREE_YEARS :: TermInYears
pattern TermInYears_THREE_YEARS = TermInYears' "THREE_YEARS"

{-# COMPLETE
  TermInYears_ONE_YEAR,
  TermInYears_THREE_YEARS,
  TermInYears'
  #-}
