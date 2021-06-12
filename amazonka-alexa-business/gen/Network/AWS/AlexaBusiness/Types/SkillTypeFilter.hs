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
-- Module      : Network.AWS.AlexaBusiness.Types.SkillTypeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillTypeFilter
  ( SkillTypeFilter
      ( ..,
        SkillTypeFilter_ALL,
        SkillTypeFilter_PRIVATE,
        SkillTypeFilter_PUBLIC
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SkillTypeFilter = SkillTypeFilter'
  { fromSkillTypeFilter ::
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
