{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeStorageClass
  ( TapeStorageClass
      ( TapeStorageClass',
        TapeStorageClassDeepArchive,
        TapeStorageClassGlacier,
        fromTapeStorageClass
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TapeStorageClass = TapeStorageClass'
  { fromTapeStorageClass ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TapeStorageClassDeepArchive :: TapeStorageClass
pattern TapeStorageClassDeepArchive = TapeStorageClass' "DEEP_ARCHIVE"

pattern TapeStorageClassGlacier :: TapeStorageClass
pattern TapeStorageClassGlacier = TapeStorageClass' "GLACIER"

{-# COMPLETE
  TapeStorageClassDeepArchive,
  TapeStorageClassGlacier,
  TapeStorageClass'
  #-}
