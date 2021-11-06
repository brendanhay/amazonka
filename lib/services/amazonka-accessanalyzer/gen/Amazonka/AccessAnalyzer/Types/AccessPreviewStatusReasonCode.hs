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
-- Module      : Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReasonCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AccessPreviewStatusReasonCode
  ( AccessPreviewStatusReasonCode
      ( ..,
        AccessPreviewStatusReasonCode_INTERNAL_ERROR,
        AccessPreviewStatusReasonCode_INVALID_CONFIGURATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AccessPreviewStatusReasonCode = AccessPreviewStatusReasonCode'
  { fromAccessPreviewStatusReasonCode ::
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

pattern AccessPreviewStatusReasonCode_INTERNAL_ERROR :: AccessPreviewStatusReasonCode
pattern AccessPreviewStatusReasonCode_INTERNAL_ERROR = AccessPreviewStatusReasonCode' "INTERNAL_ERROR"

pattern AccessPreviewStatusReasonCode_INVALID_CONFIGURATION :: AccessPreviewStatusReasonCode
pattern AccessPreviewStatusReasonCode_INVALID_CONFIGURATION = AccessPreviewStatusReasonCode' "INVALID_CONFIGURATION"

{-# COMPLETE
  AccessPreviewStatusReasonCode_INTERNAL_ERROR,
  AccessPreviewStatusReasonCode_INVALID_CONFIGURATION,
  AccessPreviewStatusReasonCode'
  #-}
