{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndexResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListIndex' response operation.
--
--
--
-- /See:/ 'batchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { _bliIndexAttachments ::
      !(Maybe [IndexAttachment]),
    _bliNextToken :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bliIndexAttachments' - The objects and indexed values attached to the index.
--
-- * 'bliNextToken' - The pagination token.
batchListIndexResponse ::
  BatchListIndexResponse
batchListIndexResponse =
  BatchListIndexResponse'
    { _bliIndexAttachments = Nothing,
      _bliNextToken = Nothing
    }

-- | The objects and indexed values attached to the index.
bliIndexAttachments :: Lens' BatchListIndexResponse [IndexAttachment]
bliIndexAttachments = lens _bliIndexAttachments (\s a -> s {_bliIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
bliNextToken :: Lens' BatchListIndexResponse (Maybe Text)
bliNextToken = lens _bliNextToken (\s a -> s {_bliNextToken = a})

instance FromJSON BatchListIndexResponse where
  parseJSON =
    withObject
      "BatchListIndexResponse"
      ( \x ->
          BatchListIndexResponse'
            <$> (x .:? "IndexAttachments" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListIndexResponse

instance NFData BatchListIndexResponse
