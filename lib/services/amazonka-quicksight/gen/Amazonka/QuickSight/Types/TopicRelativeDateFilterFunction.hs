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
-- Module      : Amazonka.QuickSight.Types.TopicRelativeDateFilterFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRelativeDateFilterFunction
  ( TopicRelativeDateFilterFunction
      ( ..,
        TopicRelativeDateFilterFunction_LAST,
        TopicRelativeDateFilterFunction_NEXT,
        TopicRelativeDateFilterFunction_NOW,
        TopicRelativeDateFilterFunction_PREVIOUS,
        TopicRelativeDateFilterFunction_THIS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicRelativeDateFilterFunction = TopicRelativeDateFilterFunction'
  { fromTopicRelativeDateFilterFunction ::
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

pattern TopicRelativeDateFilterFunction_LAST :: TopicRelativeDateFilterFunction
pattern TopicRelativeDateFilterFunction_LAST = TopicRelativeDateFilterFunction' "LAST"

pattern TopicRelativeDateFilterFunction_NEXT :: TopicRelativeDateFilterFunction
pattern TopicRelativeDateFilterFunction_NEXT = TopicRelativeDateFilterFunction' "NEXT"

pattern TopicRelativeDateFilterFunction_NOW :: TopicRelativeDateFilterFunction
pattern TopicRelativeDateFilterFunction_NOW = TopicRelativeDateFilterFunction' "NOW"

pattern TopicRelativeDateFilterFunction_PREVIOUS :: TopicRelativeDateFilterFunction
pattern TopicRelativeDateFilterFunction_PREVIOUS = TopicRelativeDateFilterFunction' "PREVIOUS"

pattern TopicRelativeDateFilterFunction_THIS :: TopicRelativeDateFilterFunction
pattern TopicRelativeDateFilterFunction_THIS = TopicRelativeDateFilterFunction' "THIS"

{-# COMPLETE
  TopicRelativeDateFilterFunction_LAST,
  TopicRelativeDateFilterFunction_NEXT,
  TopicRelativeDateFilterFunction_NOW,
  TopicRelativeDateFilterFunction_PREVIOUS,
  TopicRelativeDateFilterFunction_THIS,
  TopicRelativeDateFilterFunction'
  #-}
