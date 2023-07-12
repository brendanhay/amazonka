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
-- Module      : Amazonka.DataSync.Types.HdfsDataTransferProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.HdfsDataTransferProtection
  ( HdfsDataTransferProtection
      ( ..,
        HdfsDataTransferProtection_AUTHENTICATION,
        HdfsDataTransferProtection_DISABLED,
        HdfsDataTransferProtection_INTEGRITY,
        HdfsDataTransferProtection_PRIVACY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HdfsDataTransferProtection = HdfsDataTransferProtection'
  { fromHdfsDataTransferProtection ::
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

pattern HdfsDataTransferProtection_AUTHENTICATION :: HdfsDataTransferProtection
pattern HdfsDataTransferProtection_AUTHENTICATION = HdfsDataTransferProtection' "AUTHENTICATION"

pattern HdfsDataTransferProtection_DISABLED :: HdfsDataTransferProtection
pattern HdfsDataTransferProtection_DISABLED = HdfsDataTransferProtection' "DISABLED"

pattern HdfsDataTransferProtection_INTEGRITY :: HdfsDataTransferProtection
pattern HdfsDataTransferProtection_INTEGRITY = HdfsDataTransferProtection' "INTEGRITY"

pattern HdfsDataTransferProtection_PRIVACY :: HdfsDataTransferProtection
pattern HdfsDataTransferProtection_PRIVACY = HdfsDataTransferProtection' "PRIVACY"

{-# COMPLETE
  HdfsDataTransferProtection_AUTHENTICATION,
  HdfsDataTransferProtection_DISABLED,
  HdfsDataTransferProtection_INTEGRITY,
  HdfsDataTransferProtection_PRIVACY,
  HdfsDataTransferProtection'
  #-}
