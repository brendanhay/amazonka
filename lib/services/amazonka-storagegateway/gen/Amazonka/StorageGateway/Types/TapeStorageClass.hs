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
-- Module      : Amazonka.StorageGateway.Types.TapeStorageClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.TapeStorageClass
  ( TapeStorageClass
      ( ..,
        TapeStorageClass_DEEP_ARCHIVE,
        TapeStorageClass_GLACIER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TapeStorageClass = TapeStorageClass'
  { fromTapeStorageClass ::
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

pattern TapeStorageClass_DEEP_ARCHIVE :: TapeStorageClass
pattern TapeStorageClass_DEEP_ARCHIVE = TapeStorageClass' "DEEP_ARCHIVE"

pattern TapeStorageClass_GLACIER :: TapeStorageClass
pattern TapeStorageClass_GLACIER = TapeStorageClass' "GLACIER"

{-# COMPLETE
  TapeStorageClass_DEEP_ARCHIVE,
  TapeStorageClass_GLACIER,
  TapeStorageClass'
  #-}
