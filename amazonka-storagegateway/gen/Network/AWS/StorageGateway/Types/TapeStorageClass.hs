{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeStorageClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeStorageClass
  ( TapeStorageClass
      ( ..,
        TapeStorageClass_DEEP_ARCHIVE,
        TapeStorageClass_GLACIER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TapeStorageClass = TapeStorageClass'
  { fromTapeStorageClass ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
