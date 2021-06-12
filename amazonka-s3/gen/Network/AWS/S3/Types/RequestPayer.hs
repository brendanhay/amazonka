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
-- Module      : Network.AWS.S3.Types.RequestPayer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RequestPayer
  ( RequestPayer
      ( ..,
        RequestPayer_Requester
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

-- | Confirms that the requester knows that they will be charged for the
-- request. Bucket owners need not specify this parameter in their
-- requests. For information about downloading objects from requester pays
-- buckets, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/ObjectsinRequesterPaysBuckets.html Downloading Objects in Requestor Pays Buckets>
-- in the /Amazon S3 Developer Guide/.
newtype RequestPayer = RequestPayer'
  { fromRequestPayer ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern RequestPayer_Requester :: RequestPayer
pattern RequestPayer_Requester = RequestPayer' "requester"

{-# COMPLETE
  RequestPayer_Requester,
  RequestPayer'
  #-}
