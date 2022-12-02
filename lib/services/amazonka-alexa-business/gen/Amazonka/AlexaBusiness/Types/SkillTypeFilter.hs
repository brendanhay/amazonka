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
-- Module      : Amazonka.AlexaBusiness.Types.SkillTypeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillTypeFilter
  ( SkillTypeFilter
      ( ..,
        SkillTypeFilter_ALL,
        SkillTypeFilter_PRIVATE,
        SkillTypeFilter_PUBLIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SkillTypeFilter = SkillTypeFilter'
  { fromSkillTypeFilter ::
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

pattern SkillTypeFilter_ALL :: SkillTypeFilter
pattern SkillTypeFilter_ALL = SkillTypeFilter' "ALL"

pattern SkillTypeFilter_PRIVATE :: SkillTypeFilter
pattern SkillTypeFilter_PRIVATE = SkillTypeFilter' "PRIVATE"

pattern SkillTypeFilter_PUBLIC :: SkillTypeFilter
pattern SkillTypeFilter_PUBLIC = SkillTypeFilter' "PUBLIC"

{-# COMPLETE
  SkillTypeFilter_ALL,
  SkillTypeFilter_PRIVATE,
  SkillTypeFilter_PUBLIC,
  SkillTypeFilter'
  #-}
