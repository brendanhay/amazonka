{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.EncryptionOption
  ( EncryptionOption
    ( EncryptionOption'
    , EncryptionOptionSseS3
    , EncryptionOptionSseKms
    , EncryptionOptionCseKms
    , fromEncryptionOption
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EncryptionOption = EncryptionOption'{fromEncryptionOption
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern EncryptionOptionSseS3 :: EncryptionOption
pattern EncryptionOptionSseS3 = EncryptionOption' "SSE_S3"

pattern EncryptionOptionSseKms :: EncryptionOption
pattern EncryptionOptionSseKms = EncryptionOption' "SSE_KMS"

pattern EncryptionOptionCseKms :: EncryptionOption
pattern EncryptionOptionCseKms = EncryptionOption' "CSE_KMS"

{-# COMPLETE 
  EncryptionOptionSseS3,

  EncryptionOptionSseKms,

  EncryptionOptionCseKms,
  EncryptionOption'
  #-}
