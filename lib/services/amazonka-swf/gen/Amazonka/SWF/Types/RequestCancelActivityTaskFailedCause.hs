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
-- Module      : Amazonka.SWF.Types.RequestCancelActivityTaskFailedCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.RequestCancelActivityTaskFailedCause
  ( RequestCancelActivityTaskFailedCause
      ( ..,
        RequestCancelActivityTaskFailedCause_ACTIVITY_ID_UNKNOWN,
        RequestCancelActivityTaskFailedCause_OPERATION_NOT_PERMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RequestCancelActivityTaskFailedCause = RequestCancelActivityTaskFailedCause'
  { fromRequestCancelActivityTaskFailedCause ::
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

pattern RequestCancelActivityTaskFailedCause_ACTIVITY_ID_UNKNOWN :: RequestCancelActivityTaskFailedCause
pattern RequestCancelActivityTaskFailedCause_ACTIVITY_ID_UNKNOWN = RequestCancelActivityTaskFailedCause' "ACTIVITY_ID_UNKNOWN"

pattern RequestCancelActivityTaskFailedCause_OPERATION_NOT_PERMITTED :: RequestCancelActivityTaskFailedCause
pattern RequestCancelActivityTaskFailedCause_OPERATION_NOT_PERMITTED = RequestCancelActivityTaskFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  RequestCancelActivityTaskFailedCause_ACTIVITY_ID_UNKNOWN,
  RequestCancelActivityTaskFailedCause_OPERATION_NOT_PERMITTED,
  RequestCancelActivityTaskFailedCause'
  #-}
