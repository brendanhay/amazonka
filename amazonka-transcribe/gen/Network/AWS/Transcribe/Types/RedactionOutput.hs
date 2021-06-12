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
-- Module      : Network.AWS.Transcribe.Types.RedactionOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.RedactionOutput
  ( RedactionOutput
      ( ..,
        RedactionOutput_Redacted,
        RedactionOutput_Redacted_and_unredacted
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RedactionOutput = RedactionOutput'
  { fromRedactionOutput ::
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

pattern RedactionOutput_Redacted :: RedactionOutput
pattern RedactionOutput_Redacted = RedactionOutput' "redacted"

pattern RedactionOutput_Redacted_and_unredacted :: RedactionOutput
pattern RedactionOutput_Redacted_and_unredacted = RedactionOutput' "redacted_and_unredacted"

{-# COMPLETE
  RedactionOutput_Redacted,
  RedactionOutput_Redacted_and_unredacted,
  RedactionOutput'
  #-}
