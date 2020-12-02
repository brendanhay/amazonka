{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'DeleteObject' response operation.
--
--
--
-- /See:/ 'batchDeleteObjectResponse' smart constructor.
data BatchDeleteObjectResponse = BatchDeleteObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteObjectResponse' with the minimum fields required to make a request.
batchDeleteObjectResponse ::
  BatchDeleteObjectResponse
batchDeleteObjectResponse = BatchDeleteObjectResponse'

instance FromJSON BatchDeleteObjectResponse where
  parseJSON =
    withObject
      "BatchDeleteObjectResponse"
      (\x -> pure BatchDeleteObjectResponse')

instance Hashable BatchDeleteObjectResponse

instance NFData BatchDeleteObjectResponse
