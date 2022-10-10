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
-- Module      : Amazonka.Signer.Types.SigningStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningStatus
  ( SigningStatus
      ( ..,
        SigningStatus_Failed,
        SigningStatus_InProgress,
        SigningStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SigningStatus = SigningStatus'
  { fromSigningStatus ::
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

pattern SigningStatus_Failed :: SigningStatus
pattern SigningStatus_Failed = SigningStatus' "Failed"

pattern SigningStatus_InProgress :: SigningStatus
pattern SigningStatus_InProgress = SigningStatus' "InProgress"

pattern SigningStatus_Succeeded :: SigningStatus
pattern SigningStatus_Succeeded = SigningStatus' "Succeeded"

{-# COMPLETE
  SigningStatus_Failed,
  SigningStatus_InProgress,
  SigningStatus_Succeeded,
  SigningStatus'
  #-}
