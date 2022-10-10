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
-- Module      : Amazonka.LexV2Models.Types.PromptAttempt
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PromptAttempt
  ( PromptAttempt
      ( ..,
        PromptAttempt_Initial,
        PromptAttempt_Retry1,
        PromptAttempt_Retry2,
        PromptAttempt_Retry3,
        PromptAttempt_Retry4,
        PromptAttempt_Retry5
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The attempt name of attempts of a prompt.
newtype PromptAttempt = PromptAttempt'
  { fromPromptAttempt ::
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

pattern PromptAttempt_Initial :: PromptAttempt
pattern PromptAttempt_Initial = PromptAttempt' "Initial"

pattern PromptAttempt_Retry1 :: PromptAttempt
pattern PromptAttempt_Retry1 = PromptAttempt' "Retry1"

pattern PromptAttempt_Retry2 :: PromptAttempt
pattern PromptAttempt_Retry2 = PromptAttempt' "Retry2"

pattern PromptAttempt_Retry3 :: PromptAttempt
pattern PromptAttempt_Retry3 = PromptAttempt' "Retry3"

pattern PromptAttempt_Retry4 :: PromptAttempt
pattern PromptAttempt_Retry4 = PromptAttempt' "Retry4"

pattern PromptAttempt_Retry5 :: PromptAttempt
pattern PromptAttempt_Retry5 = PromptAttempt' "Retry5"

{-# COMPLETE
  PromptAttempt_Initial,
  PromptAttempt_Retry1,
  PromptAttempt_Retry2,
  PromptAttempt_Retry3,
  PromptAttempt_Retry4,
  PromptAttempt_Retry5,
  PromptAttempt'
  #-}
