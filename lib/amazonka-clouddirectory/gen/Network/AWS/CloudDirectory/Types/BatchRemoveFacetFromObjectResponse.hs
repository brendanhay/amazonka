{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An empty result that represents success.
--
--
--
-- /See:/ 'batchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchRemoveFacetFromObjectResponse' with the minimum fields required to make a request.
batchRemoveFacetFromObjectResponse ::
  BatchRemoveFacetFromObjectResponse
batchRemoveFacetFromObjectResponse =
  BatchRemoveFacetFromObjectResponse'

instance FromJSON BatchRemoveFacetFromObjectResponse where
  parseJSON =
    withObject
      "BatchRemoveFacetFromObjectResponse"
      (\x -> pure BatchRemoveFacetFromObjectResponse')

instance Hashable BatchRemoveFacetFromObjectResponse

instance NFData BatchRemoveFacetFromObjectResponse
