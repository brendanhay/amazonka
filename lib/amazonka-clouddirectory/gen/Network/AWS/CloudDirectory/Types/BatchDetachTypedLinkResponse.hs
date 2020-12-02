{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DetachTypedLink' response operation.
--
--
--
-- /See:/ 'batchDetachTypedLinkResponse' smart constructor.
data BatchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDetachTypedLinkResponse' with the minimum fields required to make a request.
batchDetachTypedLinkResponse ::
  BatchDetachTypedLinkResponse
batchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'

instance FromJSON BatchDetachTypedLinkResponse where
  parseJSON =
    withObject
      "BatchDetachTypedLinkResponse"
      (\x -> pure BatchDetachTypedLinkResponse')

instance Hashable BatchDetachTypedLinkResponse

instance NFData BatchDetachTypedLinkResponse
