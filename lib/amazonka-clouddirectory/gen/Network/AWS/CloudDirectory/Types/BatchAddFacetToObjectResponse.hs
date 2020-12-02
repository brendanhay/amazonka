{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The result of a batch add facet to object operation.
--
--
--
-- /See:/ 'batchAddFacetToObjectResponse' smart constructor.
data BatchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAddFacetToObjectResponse' with the minimum fields required to make a request.
batchAddFacetToObjectResponse ::
  BatchAddFacetToObjectResponse
batchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'

instance FromJSON BatchAddFacetToObjectResponse where
  parseJSON =
    withObject
      "BatchAddFacetToObjectResponse"
      (\x -> pure BatchAddFacetToObjectResponse')

instance Hashable BatchAddFacetToObjectResponse

instance NFData BatchAddFacetToObjectResponse
