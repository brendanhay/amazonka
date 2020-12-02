{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse where

import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectParentPaths' response operation.
--
--
--
-- /See:/ 'batchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { _bloppPathToObjectIdentifiersList ::
      !( Maybe
           [PathToObjectIdentifiers]
       ),
    _bloppNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectParentPathsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloppPathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- * 'bloppNextToken' - The pagination token.
batchListObjectParentPathsResponse ::
  BatchListObjectParentPathsResponse
batchListObjectParentPathsResponse =
  BatchListObjectParentPathsResponse'
    { _bloppPathToObjectIdentifiersList =
        Nothing,
      _bloppNextToken = Nothing
    }

-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
bloppPathToObjectIdentifiersList :: Lens' BatchListObjectParentPathsResponse [PathToObjectIdentifiers]
bloppPathToObjectIdentifiersList = lens _bloppPathToObjectIdentifiersList (\s a -> s {_bloppPathToObjectIdentifiersList = a}) . _Default . _Coerce

-- | The pagination token.
bloppNextToken :: Lens' BatchListObjectParentPathsResponse (Maybe Text)
bloppNextToken = lens _bloppNextToken (\s a -> s {_bloppNextToken = a})

instance FromJSON BatchListObjectParentPathsResponse where
  parseJSON =
    withObject
      "BatchListObjectParentPathsResponse"
      ( \x ->
          BatchListObjectParentPathsResponse'
            <$> (x .:? "PathToObjectIdentifiersList" .!= mempty)
            <*> (x .:? "NextToken")
      )

instance Hashable BatchListObjectParentPathsResponse

instance NFData BatchListObjectParentPathsResponse
