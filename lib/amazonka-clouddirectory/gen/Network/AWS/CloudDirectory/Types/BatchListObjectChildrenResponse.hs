{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListObjectChildren' response operation.
--
--
--
-- /See:/ 'batchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { _blocChildren ::
      !(Maybe (Map Text (Text))),
    _blocNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blocChildren' - The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- * 'blocNextToken' - The pagination token.
batchListObjectChildrenResponse ::
  BatchListObjectChildrenResponse
batchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    { _blocChildren = Nothing,
      _blocNextToken = Nothing
    }

-- | The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
blocChildren :: Lens' BatchListObjectChildrenResponse (HashMap Text (Text))
blocChildren = lens _blocChildren (\s a -> s {_blocChildren = a}) . _Default . _Map

-- | The pagination token.
blocNextToken :: Lens' BatchListObjectChildrenResponse (Maybe Text)
blocNextToken = lens _blocNextToken (\s a -> s {_blocNextToken = a})

instance FromJSON BatchListObjectChildrenResponse where
  parseJSON =
    withObject
      "BatchListObjectChildrenResponse"
      ( \x ->
          BatchListObjectChildrenResponse'
            <$> (x .:? "Children" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListObjectChildrenResponse

instance NFData BatchListObjectChildrenResponse
