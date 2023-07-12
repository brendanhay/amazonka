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
-- Module      : Amazonka.Pipes.Types.RequestedPipeStateDescribeResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.RequestedPipeStateDescribeResponse
  ( RequestedPipeStateDescribeResponse
      ( ..,
        RequestedPipeStateDescribeResponse_DELETED,
        RequestedPipeStateDescribeResponse_RUNNING,
        RequestedPipeStateDescribeResponse_STOPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RequestedPipeStateDescribeResponse = RequestedPipeStateDescribeResponse'
  { fromRequestedPipeStateDescribeResponse ::
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

pattern RequestedPipeStateDescribeResponse_DELETED :: RequestedPipeStateDescribeResponse
pattern RequestedPipeStateDescribeResponse_DELETED = RequestedPipeStateDescribeResponse' "DELETED"

pattern RequestedPipeStateDescribeResponse_RUNNING :: RequestedPipeStateDescribeResponse
pattern RequestedPipeStateDescribeResponse_RUNNING = RequestedPipeStateDescribeResponse' "RUNNING"

pattern RequestedPipeStateDescribeResponse_STOPPED :: RequestedPipeStateDescribeResponse
pattern RequestedPipeStateDescribeResponse_STOPPED = RequestedPipeStateDescribeResponse' "STOPPED"

{-# COMPLETE
  RequestedPipeStateDescribeResponse_DELETED,
  RequestedPipeStateDescribeResponse_RUNNING,
  RequestedPipeStateDescribeResponse_STOPPED,
  RequestedPipeStateDescribeResponse'
  #-}
