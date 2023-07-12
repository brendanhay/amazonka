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
-- Module      : Amazonka.AppRunner.Types.VpcIngressConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.VpcIngressConnectionStatus
  ( VpcIngressConnectionStatus
      ( ..,
        VpcIngressConnectionStatus_AVAILABLE,
        VpcIngressConnectionStatus_DELETED,
        VpcIngressConnectionStatus_FAILED_CREATION,
        VpcIngressConnectionStatus_FAILED_DELETION,
        VpcIngressConnectionStatus_FAILED_UPDATE,
        VpcIngressConnectionStatus_PENDING_CREATION,
        VpcIngressConnectionStatus_PENDING_DELETION,
        VpcIngressConnectionStatus_PENDING_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VpcIngressConnectionStatus = VpcIngressConnectionStatus'
  { fromVpcIngressConnectionStatus ::
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

pattern VpcIngressConnectionStatus_AVAILABLE :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_AVAILABLE = VpcIngressConnectionStatus' "AVAILABLE"

pattern VpcIngressConnectionStatus_DELETED :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_DELETED = VpcIngressConnectionStatus' "DELETED"

pattern VpcIngressConnectionStatus_FAILED_CREATION :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_FAILED_CREATION = VpcIngressConnectionStatus' "FAILED_CREATION"

pattern VpcIngressConnectionStatus_FAILED_DELETION :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_FAILED_DELETION = VpcIngressConnectionStatus' "FAILED_DELETION"

pattern VpcIngressConnectionStatus_FAILED_UPDATE :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_FAILED_UPDATE = VpcIngressConnectionStatus' "FAILED_UPDATE"

pattern VpcIngressConnectionStatus_PENDING_CREATION :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_PENDING_CREATION = VpcIngressConnectionStatus' "PENDING_CREATION"

pattern VpcIngressConnectionStatus_PENDING_DELETION :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_PENDING_DELETION = VpcIngressConnectionStatus' "PENDING_DELETION"

pattern VpcIngressConnectionStatus_PENDING_UPDATE :: VpcIngressConnectionStatus
pattern VpcIngressConnectionStatus_PENDING_UPDATE = VpcIngressConnectionStatus' "PENDING_UPDATE"

{-# COMPLETE
  VpcIngressConnectionStatus_AVAILABLE,
  VpcIngressConnectionStatus_DELETED,
  VpcIngressConnectionStatus_FAILED_CREATION,
  VpcIngressConnectionStatus_FAILED_DELETION,
  VpcIngressConnectionStatus_FAILED_UPDATE,
  VpcIngressConnectionStatus_PENDING_CREATION,
  VpcIngressConnectionStatus_PENDING_DELETION,
  VpcIngressConnectionStatus_PENDING_UPDATE,
  VpcIngressConnectionStatus'
  #-}
