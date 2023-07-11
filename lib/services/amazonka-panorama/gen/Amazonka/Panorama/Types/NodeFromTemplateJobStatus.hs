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
-- Module      : Amazonka.Panorama.Types.NodeFromTemplateJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeFromTemplateJobStatus
  ( NodeFromTemplateJobStatus
      ( ..,
        NodeFromTemplateJobStatus_FAILED,
        NodeFromTemplateJobStatus_PENDING,
        NodeFromTemplateJobStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeFromTemplateJobStatus = NodeFromTemplateJobStatus'
  { fromNodeFromTemplateJobStatus ::
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

pattern NodeFromTemplateJobStatus_FAILED :: NodeFromTemplateJobStatus
pattern NodeFromTemplateJobStatus_FAILED = NodeFromTemplateJobStatus' "FAILED"

pattern NodeFromTemplateJobStatus_PENDING :: NodeFromTemplateJobStatus
pattern NodeFromTemplateJobStatus_PENDING = NodeFromTemplateJobStatus' "PENDING"

pattern NodeFromTemplateJobStatus_SUCCEEDED :: NodeFromTemplateJobStatus
pattern NodeFromTemplateJobStatus_SUCCEEDED = NodeFromTemplateJobStatus' "SUCCEEDED"

{-# COMPLETE
  NodeFromTemplateJobStatus_FAILED,
  NodeFromTemplateJobStatus_PENDING,
  NodeFromTemplateJobStatus_SUCCEEDED,
  NodeFromTemplateJobStatus'
  #-}
