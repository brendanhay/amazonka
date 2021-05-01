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
-- Module      : Network.AWS.DynamoDB.Types.S3SseAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.S3SseAlgorithm
  ( S3SseAlgorithm
      ( ..,
        S3SseAlgorithm_AES256,
        S3SseAlgorithm_KMS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype S3SseAlgorithm = S3SseAlgorithm'
  { fromS3SseAlgorithm ::
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

pattern S3SseAlgorithm_AES256 :: S3SseAlgorithm
pattern S3SseAlgorithm_AES256 = S3SseAlgorithm' "AES256"

pattern S3SseAlgorithm_KMS :: S3SseAlgorithm
pattern S3SseAlgorithm_KMS = S3SseAlgorithm' "KMS"

{-# COMPLETE
  S3SseAlgorithm_AES256,
  S3SseAlgorithm_KMS,
  S3SseAlgorithm'
  #-}
