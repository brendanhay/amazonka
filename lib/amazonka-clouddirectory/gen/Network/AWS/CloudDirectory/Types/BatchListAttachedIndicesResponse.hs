{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse where

import Network.AWS.CloudDirectory.Types.IndexAttachment
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListAttachedIndices' response operation.
--
--
--
-- /See:/ 'batchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { _blaiIndexAttachments ::
      !( Maybe
           [IndexAttachment]
       ),
    _blaiNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListAttachedIndicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blaiIndexAttachments' - The indices attached to the specified object.
--
-- * 'blaiNextToken' - The pagination token.
batchListAttachedIndicesResponse ::
  BatchListAttachedIndicesResponse
batchListAttachedIndicesResponse =
  BatchListAttachedIndicesResponse'
    { _blaiIndexAttachments =
        Nothing,
      _blaiNextToken = Nothing
    }

-- | The indices attached to the specified object.
blaiIndexAttachments :: Lens' BatchListAttachedIndicesResponse [IndexAttachment]
blaiIndexAttachments = lens _blaiIndexAttachments (\s a -> s {_blaiIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
blaiNextToken :: Lens' BatchListAttachedIndicesResponse (Maybe Text)
blaiNextToken = lens _blaiNextToken (\s a -> s {_blaiNextToken = a})

instance FromJSON BatchListAttachedIndicesResponse where
  parseJSON =
    withObject
      "BatchListAttachedIndicesResponse"
      ( \x ->
          BatchListAttachedIndicesResponse'
            <$> (x .:? "IndexAttachments" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListAttachedIndicesResponse

instance NFData BatchListAttachedIndicesResponse
